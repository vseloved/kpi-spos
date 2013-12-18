(in-package :cl-user)

(print ">>> Building system....")
(require 'asdf)
(asdf:disable-output-translations)
(require-quicklisp)
(ql:quickload :hunchentoot)
(ql:quickload :rutils)
(print ">>> Done building system")

;; change the file just to invoke recompilation at heroku: 27