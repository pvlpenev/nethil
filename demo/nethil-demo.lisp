;;;; nethil-demo.lisp

(in-package #:nethil-demo)

(define-app hello
  (define-route hello ("/")
    "Hello world")
  (define-route world ("world/")
    "Works as well"))

(define-app sub
  (define-route test ("test/")
    "submodules work"))

;; (start (mount-app hello () (sub "sub" nil)))

;; now the urls /, world/ and sub/test/ will work.
