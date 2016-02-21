;;;; module.lisp

(in-package #:nethil)

(defvar *servers* nil "List of running servers")
(defvar *current-server* nil "Current server")
(defvar *apps* nil "List of defined apps")
(defvar *routes* nil "alist of defined routes")
(defvar *running-apps* nil "List of apps mounted in servers")
(defvar *env* nil "Current clack environment")
(defvar *bindings* nil "Current route bindings")


(defclass server (lack.component:lack-component)
  ((hostname :initarg :hostname :reader server-hostname)
   (port :initarg :port :reader server-port)
   (route-map :initform (make-instance 'routes:mapper) :reader server-route-map)
   (apps :initform nil :accessor server-apps)
   (handler :initarg :handler :reader server-clack-handler))
  (:documentation "Class for a server instance. K"))

(defmethod lack.component:call ((server server) env)
  (let ((*env* env))
    (multiple-value-bind (route *bindings*)
        (routes:match (server-route-map server) (getf *env* :request-uri))
      (if route
          (let ((result (funcall (route-handler route))))
            (if (stringp result)
                (list 200 (list "text/html") (list result))
                result))
          '(404 (:content-type "text/plain") ("Not Found"))))))

(defclass app ()
  ((name :initarg :name :reader app-name)
   (routes :initform nil :accessor app-routes)))

(defclass active-app (app)
  ((template :initarg :template :accessor app-template)
   (children :initform nil :initarg :app-children :accessor app-children)
   (parent :initform nil :initarg :app-parent :accessor app-parent)))

(defclass route (routes:route)
  ((app :initarg :app :reader route-app)
   (name :initarg :name :reader route-name)
   (template :initarg :template :reader route-template)
   (handler :initarg :handler :reader route-handler)))

(defmacro define-route (name (app template &key &allow-other-keys) &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (push (make-instance 'route :app ',app :name ',name :template ,template :handler ',name)
           (app-routes (find-app ',app)))))



(defun map-app-routes (app map &optional (parent-template nil))
  (let ((template (concatenate 'list parent-template (routes:parse-template (active-app-template app)))))
    (loop for route in (app-routes app)
       do (routes:connect map (concatenate 'list template (routes:parse-template (route-template route)))))
    (loop for child in (app-children)
         do (map-app-routes child map template))))

(defun mount-all-routes ()
  (loop for server in *servers*
     do (loop for app in (server-apps server)
             do (map-app-routes app (server-route-map server) (routes:parse-template (active-app-template app))))))

(defun %mount-app (app &key (mount-template nil) (parent nil) (children nil))
  (let* ((template (if parent
                       (concatenate 'list ())))
         (active-app (make-instance 'active-app
                                    :routes (app-routes app)
                                    :parent parent
                                    :template template))
         (mounted-children (loop for child in children
                              collect (mount-app (first child)
                                                 :mount-template (second child)
                                                 :parent active-app
                                                 :children (third child)))))
    (setf (app-children active-app) mounted-childer)
    active-app))


