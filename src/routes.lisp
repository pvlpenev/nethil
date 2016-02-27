;;;; routes.lisp

(in-package #:nethil)

(defclass route (routes:route)
  ((app :initarg :app :reader route-app)
   (name :initarg :name :reader route-name)
   (template :initarg :template :accessor route-template)
   (handler :initarg :handler :reader route-handler)))

(defmacro define-route (name (template &key (app nil) &allow-other-keys) &body body)
  (let ((letapp (gensym)))
   `(let ((,letapp (or ,app *app*)))
      (defun ,name ()
        ,@body)
      (setf (gethash ',name (app-routes ,letapp))
            (make-instance 'route :app ,letapp :name ',name :template ,template :handler ',name))
      ;(mount-all-routes)
      )))

(defun find-route (name app)
  (gethash name (app-routes app)))
