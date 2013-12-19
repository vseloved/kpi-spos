(ql:quickload :md5)
(ql:quickload :cl-who)
(ql:quickload :local-time)

(use-package :rutil)
(use-package :hunchentoot)
(use-package :hunch)

(eval-always (setf sb-impl::*default-external-format* :utf-8))

(defmacro void (place)
  "Nullify PLACE."
  `(setf ,place nil))

(defstruct (pair (:type list) (:conc-name nil))
  "A generic pair with left (LT) and right (RT) elements."
  lt rt)
(defun pair (x y)
  "A shortcut to make a pair of X and Y."
  (make-pair :lt x :rt y))


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
  (loop :for (beg end) :on *topics* :repeat n
     :nconc (coerce (sub (shuffle (sub *questions* beg end)) 0 2) 'list)))

(defun grade-qa (qa)
  (let ((score 0))
    (dolist (a (quest-answers (lt qa)))
      (when (and (find (sub a 2) (rt qa) :test 'string=)
                 (char= #\+ (char a 0)))
        (incf score)))
    (float (/ score
              (length (remove-if #`(char= #\- (char % 0))
                                 (quest-answers (lt qa))))))))


;;; Tries

(defvar *tries* (make-hash-table :test 'equalp))

(defstruct try
  id ts time quests qas)

(defun grade-try (try)
  (ceiling (* (/ 40 (length (try-quests try)))
              (reduce #'+ (mapcar #'grade-qa (try-qas try))))))


;;; Pages

(defparameter +center-style+
  ".center {
     margin: auto; position: absolute; top: 0; left: 0; bottom: 0;
     right: 0; display: table; height: auto;
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

(defun try-quest-grades (try)
  (who:with-html-output-to-string (out)
    (dolist (qa (reverse (try-qas try)))
      (who:htm (:li (who:fmt "~A - ~A"
                             (substr (quest-text (lt qa)) 0 -1)
                             (grade-qa qa)))))))

(defun result-page (&optional try)
  (who:with-html-output-to-string (out)
    (:html
     (:head
      (:title "Результат")
      (:style +center-style+))
     (:body
      (if try
          (who:htm
           (:div :class "center" :style "font-size: 20px;"
                 (:div (who:fmt "Ваш результат: ~A балів."
                                (grade-try try)))
                 (:ol (who:str (try-quest-grades try)))))
          (who:htm
           (:p (who:fmt "Всего результатов: ~A" (ht-count *tries*)))
           (dotable (_ try *tries*)
             (who:htm
              :br
              (:div :class "center" :style "font-size: 20px;"
                    (:div (who:fmt "[~A] ~A сек. Результат ~A: ~A балів."
                                   (try-ts try) (try-time try)
                                   (try-id try) (grade-try try)))
                    (:ol (who:str (try-quest-grades try))))))))))))


;;; Web controller

(defvar *auth-data* (make-hash-table :test 'equal))
(dolines (line (merge-pathnames "auth.txt" *load-truename*))
  (apply #`(set# % *auth-data* %%) (split #\Space line)))

(url "/" ()
  (ecase (request-method*)
    (:GET (login-page))
    (:POST
     (let* ((id (post-parameter "id"))
            (token (base64:string-to-base64-string
                    (reduce #'strcat
                            (md5:md5sum-string
                             (strcat id (princ-to-string (local-time:now))))
                            :key #'code-char))))
       (set# token *tries* (make-try :id id :quests (generate-quests)
                                     :time (get-universal-time)))
       (set-cookie "tok" :path "/" :value token)
       (redirect "/q")))))

(url "/q" ()
  (if-it (get# (cookie-in "tok") *tries*)
         (let* ((qs (try-quests it))
                (quest-pos (position-if-not 'null qs))
                (quest (elt qs quest-pos)))
           (ecase (request-method*)
             (:GET (quest-page quest-pos quest))
             (:POST (push (pair quest
                                (mapcar #'cdr (remove-if-not
                                               #`(string= "answers" (car %))
                                               (post-parameters*))))
                          (try-qas it))
                    (void (elt qs quest-pos))
                    (if (= quest-pos (1- (length qs)))
                        (progn
                          (setf (try-time it) (- (get-universal-time)
                                                 (try-time it))
                                (try-ts it) (local-time:now))
                          (redirect "/rez"))
                        (redirect "/q")))))
         (redirect "/")))

(url "/rez/:tid" (tid)
  (if (string= "all" tid)
      (mv-bind (user pass) (htt:authorization)
        (if (and pass (string= pass (get# user *auth-data*)))
            (result-page)
            (htt:require-authorization)))
      (if-it (or (get# tid *tries*)
                 (get# (cookie-in "tok") *tries*))
             (result-page it)
             (redirect "/"))))


;;; startup

(start-web)
(loop (sleep 1000))
