(ql:quickload '#:cl-ppcre)
(ql:quickload '#:rutils)
(ql:quickload '#:hunchentoot)
(ql:quickload '#:named-readtables)

(cl:rename-package "CL-PPCRE" "CL-PPCRE" '("PPCRE" "RE"))
(cl:rename-package "HUNCHENTOOT" "HUNCHENTOOT" '("TBNL" "HTT"))


(defpackage #:hunch
  (:use #:cl #:rutil)
  (:import-from #:hunchentoot #:acceptor-log-message)
  (:export #:*hunch-acceptor*
           #:*port*
           #:*swank-port*
           #:*debug*
           #:start-web
           #:restart-web
           #:stop-web
           #:uri
           #:fmt-uri
           #:list-uris
           #:parse-uri-template
           #:argv))

(in-package #:hunch)
(named-readtables:in-readtable rutils-readtable)


;;; config

(defun argv ()
  "List of program's command-line arguments."
  #+:sbcl (nthcdr 2 sb-ext:*posix-argv*)
  #+:ccl (nthcdr 4 ccl:*command-line-argument-list*))

(defvar *hunch-acceptor* nil
  "Hunch acceptor.")
(defvar *port* 8080
  "Port at which the application will be started.")
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


;;; URL routing

(defun parse-uri-template (uri)
  "Split URI into parts of 2 types:
   - constant string
   - uri parameter names (symbols)"
  (let ((prev 0)
        parts)
    (do ((pos (position #\: uri) (position #\: uri :start prev)))
        ((or (null pos) (= pos prev))
         (push (sub uri prev) parts))
      (unless (= prev pos)
        (push (sub uri prev pos) parts))
      (setf prev (position-if #`(char= #\/ %) uri :start (1+ pos)))
      (push (mksym (sub uri (1+ pos) prev)) parts)
      (if prev
          (when (= prev (1+ pos))
            (error "Param name is blank in hunch uri definition at: ~A" pos))
          (return)))
    (reverse parts)))

(defmacro uri (uri-template (&rest params) &body body)
  "Define a handler function for URI-TEMPLATE
   on top of HUNCHENTOOT:DEFINE-EASY-HANDLER.
   The function will be called after HANDLE + <URI-TEMPLATE>
   (like HANDLE-/FOO for uri '/foo'). If the URI contains parameter names
   (basically keywords, like in '/foo/:bar/baz' bar will be a parameter name)
   they may be referenced in easy-handler uri parameters."
  (with-gensyms (req uri parts cur pos end)
    (let ((uri-parts (parse-uri-template uri-template)))
      `(htt:define-easy-handler
           (,(mksym uri-template :format "handle-~A")
            :uri ,(if (rest uri-parts)
                      `(lambda (,req)
                         (let ((,pos 0)
                               (,uri (htt:request-uri ,req)))
                           (loop :for ,parts :on ',uri-parts :do
                              (let ((,cur (first ,parts)))
                                (if (stringp ,cur)
                                    (let ((,end (mismatch ,cur ,uri :start2 ,pos)))
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
                                        (if-it (search (second ,parts) ,uri
                                                       :start2 ,pos)
                                               (setf ,end it
                                                     ,parts (rest ,parts))
                                               (return)))
                                      (push (cons (string-downcase (string ,cur))
                                                  (sub ,uri ,pos ,end))
                                            (slot-value ,req 'htt:get-parameters))
                                      (if (rest ,parts)
                                          (setf ,pos ,end)
                                          (return t))))))))
                      uri-template))
           (,@params)
         ,@body))))

(defmacro fmt-uri (handler &rest args &key &allow-other-keys)
  "Return a string representation of HANDLER's uri
   with uri-parameters substitutted for values of ARGS.
   If some parameter is missing, UNBOUND-VARIABLE will be signalled."
  `(let (,@(loop :for (var val) :on args :by #'cddr
              :collect (list (mksym var) val)))
     (strcat ,@(parse-uri-template (sub (symbol-name handler)
                                        #.(length "handle-"))))))

(defun print-uris ()
  "Print defined uris with their handler functions."
  (dolist (record htt::*easy-handler-alist*)
    (format t "~A ~A~%" (first record) (third record))))


;;; main

(load *script* :external-format :utf-8)
