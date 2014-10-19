;;;; package.lisp

(defpackage #:nethil
  (:use #:cl)
  (:export #:define-route
           #:start
           #:*request*
           #:*env*
           #:*bindings*))

