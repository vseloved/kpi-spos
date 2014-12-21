(ql:quickload :md5)
(ql:quickload :cl-who)
(ql:quickload :local-time)

(use-package :rutil)
(use-package :hunchentoot)
(use-package :hunch)

(eval-always (setf sb-impl::*default-external-format* :utf-8))


;;; Questions

(defstruct quest
  text answers)

(defvar *questions* (make-array 0 :adjustable t :fill-pointer t))
(defvar *topics* ())

(eval-always
  (let (cur)
    (dolines (line (merge-pathnames "quests.txt" *load-truename*))
      (if (or (blankp line)
              (member (char line 0) '(#\Space #\Tab #\#)))
          (progn
            (when (and (not (blankp line))
                       (char= (char line 0) #\#))
              (push (length *questions*) *topics*))
            (when cur
              (vector-push-extend cur *questions*)
              (void cur)))
          (if cur
              (push line (quest-answers cur))
              (setf cur (make-quest :text line)))))
    (vector-push-extend cur *questions*)
    (reversef *topics*)))

(defun generate-quests (&optional (n 10))
  (let ((by-topic (floor n (length *topics*))))
    (loop :for (beg end) :on *topics* :repeat n
       :nconc (coerce (sub (shuffle (sub *questions* beg end)) 0 by-topic)
                      'list))))

(defun grade-qa (qa)
  (let ((score 0))
    (dolist (a (quest-answers (lt qa)))
      (when (and (find (sub a 2) (rt qa) :test 'string=)
                 (char= #\+ (char a 0)))
        (incf score)))
    (float (/ score
              (length (remove-if #`(char= #\- (char % 0))
                                 (quest-answers (lt qa))))))))


;;; Sessions

(defvar *sessions* (make-hash-table :test 'equalp))

(defstruct session
  id ts time quests qas)

(defun grade-session (session)
  (ceiling (* (/ 40 (length (session-quests session)))
              (reduce #'+ (mapcar #'grade-qa (session-qas session))))))


;;; Pages

(defparameter +center-style+
  ".center {
     margin: auto; position: absolute; top: 0; left: 0; bottom: 0;
     right: 0; display: table; height: 50%; width: 50%;
     overflow: auto;
   }")

(defun login-page ()
  (who:with-html-output-to-string (out)
    (:html
     (:head
      (:title "Вхід")
      (:style +center-style+))
     (:body
      (:form :class "center" :method "POST"
             (:label :name "id" "Номер залікової книжки: ")
             (:input :name "id")
             (:input :type "submit" :value "Увійти"))))))

(defun quest-page (qid quest)
  (who:with-html-output-to-string (out)
    (:html
     (:head
      (:title "Питання")
      (:style +center-style+))
     (:body
      (:form :class "center" :method "POST"
             (:label (who:fmt (quest-text quest)))
             (:input :name "qid" :type "hidden" :value qid)
             :br
             (dolist (answer (shuffle (quest-answers quest)))
               (who:htm (:input :name "answers" :type "checkbox"
                                :value (sub answer 2)
                                (who:str (sub answer 2))) :br))
             (:input :type "submit" :value "Відправити"))))))

(defun session-grade-block (session &optional detailed)
  (who:with-html-output-to-string (out)
    (dolist (qa (reverse (session-qas session)))
      (who:htm (:li (who:fmt "~A - ~A"
                             (substr (quest-text (lt qa)) 0 -1)
                             (grade-qa qa))
                    (when detailed
                      (dolist (a (quest-answers (lt qa)))
                        (when (char= #\+ (char a 0))
                          (who:htm
                           :br
                           (who:fmt "~:[-~;+~] ~A"
                                    (find (sub a 2) (rt qa) :test 'string=)
                                    (sub a 2)))))))))))

(defun result-page (&optional session detailed)
  (who:with-html-output-to-string (out)
    (:html
     (:head
      (:title "Результат")
      (:style +center-style+))
     (:body
      (if session
          (who:htm
           (:div :class "center" :style "font-size: 20px;"
                 (:div (who:fmt "Результат: ~A балів."
                                (grade-session session)))
                 (:ol (who:str (session-grade-block session detailed)))))
          (who:htm
           (:p (who:fmt "Всього результатів: ~A" (ht-count *sessions*)))
           (dotable (tok session *sessions*)
             (who:htm
              :br
              (:div :class "center" :style "font-size: 20px;"
                    (:div "[" (:a :href (fmt "/rez/~A" tok) (session-ts session)) "]"
                          (who:fmt " ~A сек. Результат ~A: ~A балів."
                                   (session-time session) (session-id session)
                                   (grade-session session)))
                    (:ol (who:str (session-grade-block session))))))))))))


;;; Web controller

(defvar *auth-data* (make-hash-table :test 'equal))
(dolines (line (merge-pathnames "auth.txt" *load-truename*))
  (apply #`(set# % *auth-data* %%) (split #\Space line)))

(uri "/" ()
  (ecase (request-method*)
    (:GET (login-page))
    (:POST
     (let* ((id (post-parameter "id"))
            (token (base64:string-to-base64-string
                    (reduce #'strcat
                            (md5:md5sum-string
                             (strcat id (princ-to-string (local-time:now))))
                            :key #'code-char))))
       (set# token *session* (make-session :id id :quests (generate-quests)
                                           :time (get-universal-time)))
       (set-cookie "tok" :path "/" :value token)
       (redirect "/q")))))

(uri "/q" ()
  (if-it (get# (cookie-in "tok") *sessions*)
         (let* ((qs (session-quests it))
                (quest-pos (position-if-not 'null qs))
                (quest (elt qs quest-pos)))
           (ecase (request-method*)
             (:GET (quest-page quest-pos quest))
             (:POST (push (pair quest
                                (mapcar #'cdr (remove-if-not
                                               #`(string= "answers" (car %))
                                               (post-parameters*))))
                          (session-qas it))
                    (void (elt qs quest-pos))
                    (if (= quest-pos (1- (length qs)))
                        (progn
                          (setf (session-time it) (- (get-universal-time)
                                                     (session-time it))
                                (session-ts it) (local-time:now))
                          (redirect "/rez"))
                        (redirect "/q")))))
         (redirect "/")))

(uri "/rez/:tid" (tid)
  (if (string= "all" tid)
      (mv-bind (user pass) (htt:authorization)
        (if (and pass (string= pass (get# user *auth-data*)))
            (result-page nil t)
            (htt:require-authorization)))
      (if-it (or (get# tid *sessions*)
                 (get# (cookie-in "tok") *session*))
             (result-page it (in# tid *sessions*))
             (redirect "/"))))


;;; startup

(start-web)
(loop (sleep 1000))
