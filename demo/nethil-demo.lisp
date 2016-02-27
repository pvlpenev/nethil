;;;; nethil-demo.lisp

(in-package #:nethil-demo)

(define-site demo :hostname "localhost")

(define-app hello)
(define-app sub)

(with-app (hello)
  (define-route hello ("/")
      "Hello world")
  (define-route world ("world/")
      "Works as well"))

(with-app (sub)
  (define-route test ("test/")
      "submodules work"))

;; To run demo(must be in nethil-demo package):

;; (start (compose demo (hello "/" ((sub "sub")))))

;; now the urls /, world/ and sub/test/ will work.
