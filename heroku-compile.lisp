(in-package :cl-user)

(print ">>> Building system....")
(require 'asdf)
(asdf:disable-output-translations)
(require-quicklisp)
(ql:quickload :hunchentoot)
(ql:quickload :rutils)
(ql:quickload :swank)
(ql:quickload :cl-ppcre)
(ql:quickload :named-readtables)
(print ">>> Done building system")

;; change the file just to invoke recompilation at heroku: 27