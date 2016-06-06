;;;; core.lisp
;;; 
;;; This file defines the core of Nethil
;;;

(cl:defpackage :nethil.core
  (:use #:cl)
  (:export
   ;; Globals
   #:*site*
   #:*app*
   #:*controller*
   #:*env*
   #:*bindings*
   ;; App
   #:app
   #:app-name
   #:find-app
   #:app-controllers
   #:define-app
   ;; Controller
   #:controller
   #:controller-app
   #:controller-name
   #:controller-handler
   #:find-controller
   #:add-controller
   #:define-controller
   ;; Site
   #:site
   #:site-name
   #:site-hostname
   #:site-port
   #:site-route-map
   #:site-clack-handler
   #:define-site
   #:find-site
   #:start
   #:stop
   #:stop-all
   ;; Route
   #:route
   #:route-name
   #:route-controller
   #:mount-route))

(cl:in-package :nethil.core)

;;; Global vars

(defvar *sites* (make-hash-table) "List of active sites")
(defvar *site* nil "Current active site")

(defvar *apps* (make-hash-table) "List of defined apps")
(defvar *app*  "Current app")

(defvar *controllers* (make-hash-table) "list of defined controllers")
(defvar *controller* nil "Current controller")

(defvar *env* nil "Current clack environment")
(defvar *bindings* nil "Current route bindings")

;;; App

(defclass app ()
  ((name :initarg :name :reader app-name)
   (controllers :initarg :controllers :initform (make-hash-table) :accessor app-controllers))
  (:documentation "A collection of controllers"))

(defun find-app (name)
  "Find an app in the global registry"
  (gethash name *apps* nil))

(defmacro define-app (name &body options)
  "Define an app and add it to the global registry"
  (declare (ignore options))
  `(setf (gethash ',name *apps*) (make-instance 'app :name ',name)))

;;; Controller

(defclass controller ()
  ((app :initarg :app :reader controller-app)
   (name :initarg :name :reader controller-name)
   (handler :initarg :handler :accessor controller-handler))
  (:documentation "Controllers proccess requests and return responses"))

(defun find-controller (name &optional (app nil))
  "Find a controller in APP, or if APP is nil, in the global registry"
  (gethash name (if app
                    (app-controllers app)
                    *controllers*)
           nil))

(defun add-controller (controller app)
  "Add a controller to an app, and in the global registry"
  (setf (gethash (controller-name controller) (app-controllers app)) controller
        (gethash (controller-name controller) *controllers*) controller))

(defmacro define-controller (name (&key (app *app*) &allow-other-keys) &body body)
  "Define a controller and add it to APP and global registry"
  `(progn
     (defun ,name () ;; TODO: Parse template and determine function parameters
       ,@body)
     (add-controller (make-instance 'controller
                                    :app ,app
                                    :name ',name
                                    :handler ',name)
                     ,app)))

;;; Site

(defclass site (lack.component:lack-component)
  ((name :initarg :name :reader site-name)
   (hostname :initarg :hostname :reader site-hostname)
   (port :initarg :port :reader site-port)
   (route-map :initform (make-instance 'routes:mapper) :reader site-route-map)
   (handler :initform nil :accessor site-clack-handler))
  (:documentation "Class for a server instance."))

(defmethod lack.component:call ((site site) env)
  (let* ((*env* env)
         (*site* site))
    (multiple-value-bind (route *bindings*)
        (routes:match (site-route-map site) (getf *env* :request-uri))
      (if route
          (let* ((*controller* (route-controller route))
                 (*app* (controller-app *controller*))
                 (result (funcall (route-handler route))))
            (if (stringp result)
                (list 200 (list "text/html") (list result))
                result))
          '(404 (:content-type "text/plain") ("Not Found"))))))

(defmacro define-site (name &key (hostname "localhost") (port 8080))
  "Define a site and add it to the global registry"
  `(setf (gethash ',name *sites*)
         (make-instance 'site
                        :name ',name
                        :hostname ,hostname
                        :port ,port)))

(defun find-site (name)
  "Find site in the global registry"
  (gethash name *sites* nil))

(defgeneric start (site)
  (:documentation "Start a server instance of SITE"))

(defmethod start ((name symbol))
  (start (find-site name)))

(defmethod start ((site site))
  (setf (site-clack-handler site)
        (clack:clackup site
                       :hostname (site-hostname site)
                       :port (site-port site))))

(defgeneric stop (site)
  (:documentation "Stop a server instance of SITE"))

(defmethod stop ((name symbol))
  (stop (find-symbol name)))

(defmethod stop ((site site))
  (clack:stop (site-clack-handler site)))

(defun stop-all ()
  "Stop all sites"
  (loop for site being the hash-values of *sites*
       do (stop site)))

;;; Routes

(defclass route (routes:route)
  ((name :initarg :name :reader route-name)
   (controller :initarg :controller :reader route-controller)
   (handler :initarg :handler :reader route-handler))
  (:documentation "Routes define a mapping from a URL template to a controller"))

(defgeneric mount-route (site name template controller)
  (:documentation "Add a route to a site"))

(defmethod mount-route ((site symbol) name template (controller symbol))
  (mount-route (find-site site) name template (find-controller controller)))

(defmethod mount-route ((site site) name template (controller controller))
  (let ((route (make-instance 'route
                              :name name
                              :template (routes:parse-template template)
                              :controller controller
                              :handler (controller-handler controller)))) ;; Optimization
    (routes:connect (site-route-map site) route)))
