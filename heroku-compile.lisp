(in-package :cl-user)

(princ ">>> Building system....")

(require 'asdf)
;; (asdf:initialize-output-translations
;;  `(:output-translations (T ,(sb-ext:posix-getenv "CACHE_DIR"))))

(setf sb-impl::*default-external-format* :utf-8)

(format t "~&!!! ASDF version: ~A" (asdf:asdf-version))
(format t "~&!!! SBCL encoding: ~A" sb-impl::*default-external-format*)

(require-quicklisp)
(ql:quickload :cl-ppcre)
(ql:quickload :hunchentoot)
(ql:quickload :rutils)
(ql:quickload :swank)
(ql:quickload :named-readtables)
(ql:quickload :cl-who)
(ql:quickload :md5)
(ql:quickload :local-time)

(princ ">>> Done building system")

;; change the file just to invoke recompilation at heroku: 27