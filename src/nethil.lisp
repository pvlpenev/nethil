;;;; nethil.lisp

(in-package #:nethil)

;;; "nethil" goes here. Hacks and glory await!

(defvar *request* nil "Current request")

(defvar *env* nil "Current clack environment")

(defvar *bindings* nil "Current route bindings")

(defvar *mapper* (make-instance 'routes:mapper) "Routes mapper")

(defclass route (routes:route)
  ((name :initarg :name :reader route-name)
   (handler :initarg :handler :reader route-handler)))

(defmacro define-route (name template &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (routes:connect *mapper* (make-instance 'route
                                             :template (routes:parse-template ,template)
                                             :handler ',name))))

(defun start  (&key (port 8000))
  (clack:clackup
   #'(lambda (env)
       (let ((*env* env)
             (*request* (clack.request:make-request env)))
         (multiple-value-bind (route *bindings*)
             (routes:match *mapper* (clack.request:request-uri *request*))
           (if route
               (let ((result (funcall (route-handler route))))
                 (if (stringp result)
                     (list 200 (list "text/html") (list result))
                     result))
               '(404 (:content-type "text/plain") ("Not Found"))))))
   :port port))
