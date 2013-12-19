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

(defun grade (qa)
  (let ((score 0))
    (dolist (a (quest-answers (lt qa)))
      (when (and (find (sub a 2) (rt qa) :test 'string=)
                 (char= #\+ (char a 0)))
        (incf score)))
    (float (/ score
              (length (remove-if #`(char= #\- (char % 0))
                                 (quest-answers (lt qa))))))))


;;; Web controller

(defvar *tries* (make-hash-table :test 'equalp))
(defvar *results* ())

(defstruct try
  id quests qas)

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
       (set# token *tries* (make-try :id id :quests (generate-quests)))
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
                        (progn (push (pair it (local-time:now)) *results*)
                               (redirect "/rez"))
                        (redirect "/q")))))
         (redirect "/")))

(url "/rez/:tid" (tid)
  (if-it (or (get# tid *tries*)
             (get# (cookie-in "tok") *tries*))
         (result-page it)
         (redirect "/")))


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
             (dolist (answer (quest-answers quest))
               (who:htm (:input :name "answers" :type "checkbox"
                                :value (sub answer 2)
                                (who:str (sub answer 2))) :br))
             (:input :type "submit" :value "Відправити"))))))

(defun result-page (try)
  (let ((coef (/ 40 (length (try-quests try)))))
    (who:with-html-output-to-string (out)
      (:html
       (:head
        (:title "Результат")
        (:style +center-style+))
       (:body
        (:div :class "center" :style "font-size: 20px;"
              (:div (who:fmt "Ваш результат: ~A балів."
                             (ceiling (* coef
                                         (reduce #'+ (mapcar #'grade
                                                             (try-qas try)))))))
              (:ol
               (dolist (qa (try-qas try))
                 (who:htm (:li (who:fmt "~A - ~A"
                                        (substr (quest-text (lt qa)) 0 -1)
                                        (grade qa))))))))))))

(setf htt:*header-stream* *standard-output*)

;;; startup

(start-web)
(loop (sleep 1000))