;;;; package.lisp

(defpackage #:nethil
  (:use #:cl)
  (:export #:site
           #:define-site
           #:find-site-by-name
           #:find-site
           #:mount-site-routes
           #:mount-site-apps
           #:mount-all-routes
           #:compose
           #:start
           #:stop
           #:stop-all
           
           ;; globals
           #:*site*
           #:*app*
           #:*env*
           #:*bindings*
           
           ;; app
           #:app
           #:find-app
           #:define-app
           #:with-app
           #:mount-app
           
           ;; routes
           #:route
           #:define-route
           #:find-route))
