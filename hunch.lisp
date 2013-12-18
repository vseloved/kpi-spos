(ql:quickload '#:rutils)
(ql:quickload '#:hunchentoot)
(ql:quickload '#:named-readtables)
(ql:quickload '#:swank)

(cl:rename-package "CL-PPCRE" "CL-PPCRE" '("PPCRE" "RE"))
(cl:rename-package "HUNCHENTOOT" "HUNCHENTOOT" '("TBNL" "HTT"))


(defpackage #:hunch
  (:use #:cl #:rutil)
  (:import-from #:hunchentoot #:acceptor-log-message)
  (:export #:*hunch-acceptor*
           #:*port*
           #:*swank-port*
           #:*debug*
           #:url
           #:list-urls
           #:start-web
           #:restart-web
           #:stop-web
           #:start-swank
           #:stop-swank
           #:argv
           #:parse-url-template))

(in-package #:hunch)
(named-readtables:in-readtable rutils.readtable:rutils-readtable)


;;; config

(defun argv ()
  "List of program's command-line arguments."
  #+:sbcl (nthcdr 2 sb-ext:*posix-argv*)
  #+:ccl (nthcdr 4 ccl:*command-line-argument-list*))

(defvar *hunch-acceptor* nil
  "Hunch acceptor.")
(defvar *port* 8080
  "Port at which the application will be started.")
(defvar *swank-port* nil
  "Port for starting swank. If nil swank won't be started.")
(defvar *script* nil
  "Script file to load.")
(defvar *debug* nil
  "Start in debug mode.")

;; configure vars from command line
(loop :for args :on (argv) :do
   (when (char= #\- (char (first args) 0))
     (setf (symbol-value (mksym (sub (first args) 1) :format "*~A*"))
                (let ((val (second args)))
                  (if (digit-char-p (char val 0 ))
                      (read-from-string val)
                      val)))))

(setf htt:*show-lisp-errors-p* *debug*)


;;; web

(defun start-web (&optional port)
  (setf *hunch-acceptor* (make-instance 'htt:easy-acceptor
                                        :port (or port *port*)
                                        :error-template-directory nil))
  (bt:make-thread #`(htt:start *hunch-acceptor*) :name "hunch-acceptor")
  (acceptor-log-message *hunch-acceptor* :info
                        "Started hunch acceptor at port: ~A." (or port *port*)))

(defun stop-web ()
  (when *hunch-acceptor*
    (ignore-errors (htt:stop *hunch-acceptor*)))
  (mapc #'bt:destroy-thread
        (remove-if-not #`(member (bt:thread-name %) '("hunch-")
                                 :test #`(starts-with %% %))
                       (bt:all-threads)))
  (acceptor-log-message *hunch-acceptor* :info
                        "Stopped hunch acceptor at port: ~A."
                        (htt:acceptor-port *hunch-acceptor*)))

(defun restart-web (&optional port)
  (stop-web)
  (sleep 0.1)
  (start-web port))


;;; swank

(defun start-swank (&optional port)
  (when-it (or port *swank-port*)
    (let ((*debug-io* (make-broadcast-stream)))
      (swank:create-server :port it
                           :dont-close t)
      (acceptor-log-message *hunch-acceptor* :info
                            "Started swank at port: ~A." it))))

(defun stop-swank (&optional port)
  (when-it (or port *swank-port*)
    (swank:stop-server it)
    (acceptor-log-message *hunch-acceptor* :info "Stopped swank at port: ~A." it)))


;;; URL routing

(defun parse-url-template (url)
  "Split URL into parts of 2 types:
   - constant string
   - url parameter names (symbols)"
  (let ((prev 0)
        parts)
    (do ((pos (position #\: url) (position #\: url :start prev)))
        ((or (null pos) (= pos prev))
         (push (sub url prev) parts))
      (unless (= prev pos)
        (push (sub url prev pos) parts))
      (setf prev (position-if #`(char= #\/ %) url :start (1+ pos)))
      (push (mksym (sub url (1+ pos) prev)) parts)
      (if prev
          (when (= prev (1+ pos))
            (error "Param name is blank in hunch url definition at: ~A" pos))
          (return)))
    (reverse parts)))

(defmacro url (url-template (&rest params) &body body)
  "Define a handler function for URL-TEMPLATE
   on top of HUNCHENTOOT:DEFINE-EASY-HANDLER.
   The function will be called after the URL-TEMPLATE
   (like |/foo| for url '/foo'). If the URL contains parameter names
   (basically keywords, like in '/foo/:bar/baz' bar will be a pramneter name)
   they may be referenced in easy-handler url parameters."
  (with-gensyms (req url parts cur pos end)
    (let ((url-parts (parse-url-template url-template)))
      `(htt:define-easy-handler
           (,(mksym url-template :format "handle-~A")
            :uri ,(if (rest url-parts)
                      `(lambda (,req)
                         (let ((,pos 0)
                               (,url (htt:request-uri ,req)))
                           (loop :for ,parts :on ',url-parts :do
                              (let ((,cur (first ,parts)))
                                (if (stringp ,cur)
                                    (let ((,end (mismatch ,cur ,url :start2 ,pos)))
                                      (cond
                                        ((or (not ,end)
                                             (string= "/" (sub ,cur ,end)))
                                         (return (not (or (cddr ,parts)
                                                          (stringp
                                                           (second ,parts))))))
                                        ((= ,end (length ,cur))
                                         (incf ,pos ,end))
                                        (t (return))))
                                    (let (,end)
                                      (when (rest ,parts)
                                        (if-it (search (second ,parts) ,url
                                                       :start2 ,pos)
                                               (setf ,end it
                                                     ,parts (rest ,parts))
                                               (return)))
                                      (push (cons (string-downcase (string ,cur))
                                                  (sub ,url ,pos ,end))
                                            (slot-value ,req 'htt:get-parameters))
                                      (if (rest ,parts)
                                          (setf ,pos ,end)
                                          (return t))))))))
                      url-template))
           (,@params)
         ,@body))))

(defun print-urls ()
  "Print defined urls with their handler functions."
  (dolist (record htt::*easy-handler-alist*)
    (format t "~A ~A~%" (first record) (third record))))


;;; main

(load *script*)
