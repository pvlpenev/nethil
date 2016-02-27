;;;; app.lisp

(in-package #:nethil)

(defclass app ()
  ((name :initarg :name :reader app-name)
   (routes :initform (make-hash-table) :accessor app-routes)
   (template :initarg :template :initarg nil :accessor app-template)
   (children :initform nil :accessor app-children)
   (parent :initform nil :accessor app-parent)))

(defun find-app (name)
  (find name *apps* :key #'app-name :test #'eql))

(defun define-app (name)
  `(pushnew (make-instance 'app :name ',name) *apps*))

(defmacro with-app ((name) &body body)
  `(let ((*app* (find-app ',name)))
     ,@body))

(defun map-app-routes (app map)
  (loop for route in (app-routes app)
     do (routes:connect map route))
  (loop for child in (app-children app)
     do (map-app-routes child map)))

(defun mount-app (app &optional (mount-template nil) (children nil) (parent nil))
  (let* ((app (if (symbolp app)
                  (find-app app)
                  app))
         (template (if parent
                       (concatenate 'list
                                    (app-template parent)
                                    (routes:parse-template mount-template))
                       mount-template))
         (active-app (make-instance 'app
                                    :parent parent
                                    :template template
                                    :routes (loop for route in (app-routes app)
                                               collect (make-instance 'route
                                                                      :app app
                                                                      :name (route-name route)
                                                                      :handler (route-handler route)
                                                                      :template (concatenate 'list
                                                                                             template
                                                                                             (routes:parse-template (route-template route)))))))
         (mounted-children (loop for child in children
                              collect (mount-app (first child)
                                                 (second child)
                                                 (third child)
                                                 active-app))))
    (setf (app-children active-app) mounted-children)
    active-app))
