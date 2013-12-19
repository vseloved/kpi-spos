(in-package :cl-user)

(print ">>> Building system....")

(require 'asdf)
;; (asdf:initialize-output-translations
;;  `(:output-translations (T ,(sb-ext:posix-getenv "CACHE_DIR"))))

(require-quicklisp)
(ql:quickload :cl-ppcre)
(ql:quickload :hunchentoot)
(ql:quickload :rutils)
(ql:quickload :swank)
(ql:quickload :named-readtables)
(ql:quickload :cl-who)
(ql:quickload :md5)
(ql:quickload :local-time)

(print ">>> Done building system")

;; change the file just to invoke recompilation at heroku: 27