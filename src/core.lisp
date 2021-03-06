;;;; core.lisp
;;; 
;;; This file defines the core of Nethil
;;;

(cl:defpackage :nethil.core
  (:use #:cl)
  (:export
   ;; Globals
   #:*site*
   #:*route*
   #:*request*
   #:*env*
   #:*bindings*
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
   #:route-handler
   #:route-method
   #:route-conditions
   #:process-route
   #:make-route
   #:add-routes))

(cl:in-package :nethil.core)

;;; Global vars

(defvar *sites* (make-hash-table) "List of active sites")
(defvar *site* nil "Current active site")

(defvar *routes* nil "Registered routes") ;; For debug

(defvar *route* nil "Current route")
(defvar *request* nil "current request")
(defvar *env* nil "Current clack environment")
(defvar *bindings* nil "Current route bindings")

;;; Site

(defclass site (lack.component:lack-component)
  ((name :initarg :name :reader site-name)
   (hostname :initarg :hostname :reader site-hostname)
   (port :initarg :port :reader site-port)
   (route-map :initform (make-instance 'routes:mapper) :reader site-route-map)
   (handler :initform nil :accessor site-clack-handler))
  (:documentation "Class for a server instance."))

(defmethod lack.component:call ((site site) env)
  (let* ((*request* (lack.request:make-request env))
         (*site* site))
    (multiple-value-bind (route *bindings*)
        (routes:match (site-route-map site) (lack.request:request-uri *request*))
      (if route
          (let* ((result (process-route route *bindings*)))
            (if (stringp result)
                (list 200 (list "text/html") (list result))
                result)) ;; assume route handlers return either a string or valid LACK response.
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
  (stop (find-site name)))

(defmethod stop ((site site))
  (clack:stop (site-clack-handler site)))

(defun stop-all ()
  "Stop all sites"
  (loop for site being the hash-values of *sites*
       do (stop site)))

;;; Routes

(defclass route (routes:route)
  ((name :initarg :name :reader route-name)
   (method :initarg :method :reader route-method)
   (conditions :initarg :conditions :initform nil :reader route-conditions)
   (additional-parameters :initarg :additional-parameters :initform nil :reader route-additional-parameters)
   (handler :initarg :handler :reader route-handler))
  (:documentation "Routes define a mapping from a URL template to a controller"))

(defmethod routes:route-check-conditions ((route route) bindings)
  (with-slots (method conditions) route
    (and (if method
             (eql (lack.request:request-method *request*) method)
             t)
         (if conditions
             (every #'funcall conditions)
             t))))

(defgeneric process-route (route bindings)
  (:documentation "Process route after it has been matched to a URI, apply handler to bindings.
The route instance is bound to *ROUTE* variable during processing"))

(defmethod process-route ((route route) bindings)
  (let ((*route* route))
    (apply (route-handler route)
           (concatenate 'list 
                        (alexandria:alist-plist bindings)
                        (route-additional-parameters route)))))

(defun route (name template handler &key (method :get) (conditions nil) (additional-parameters nil) &allow-other-keys)
  "Make an instance of ROUTE. The function takes the following parameters:
NAME: symbol, names the route
TEMPLATE: string, any route template accepted by cl-routes
METHOD: keyword, HTTP method matched by the route
HANDLER: symbol or function, function handling the request.
         Must take the same keyword arguments as defined in TEMPLATE
&optional CONDITIONS: list, a list of functions to be called while matching route,
                      if any return NIL, route will not match"
  (make-instance 'route
                 :name name
                 :template (routes:parse-template template)
                 :method method
                 :additional-parameters additional-parameters
                 :handler handler
                 :conditions conditions))

(defun include (routes &key (name-prefix nil) (template nil))
  "Add a prefix to route template and route name to routes."
  (loop for route in routes
     collect (destructuring-bind (route-name route-template route-handler &key (method nil) (conditions nil) (additional-parameters nil))
                 route
               (route (intern (concatenate 'string
                                           (symbol-name name-prefix)
                                           (symbol-name route-name))
                              (symbol-package name-prefix)) ;; Make a new name for the route for reverse lookup. 
                      (concatenate 'string
                                   template
                                   route-template)
                      route-handler
                      :method method
                      :conditions conditions
                      :additional-parameters additional-parameters))))

(defgeneric add-routes (site route-specs)
  (:documentation "Add routes to SITE. Takes the fowolling parameters:
SITE: symbol or site: The site object
ROUTE-SPECS: list, list of route specs, same as arguments to MAKE-ROUTE, ROUTE-SPECS can also be nested, ADD-ROUTES will recurse down in that case.

Example:
(add-routes 'mysite
            '((index \"/\" :get index)
              (hello \"/hello/:name\" :get hello)))
Note the function hello must have the following LAMBDA-LIST: (&key hello) or (&key &allow-other-keys)"))

(defmethod add-routes ((site symbol) route-specs)
  (add-routes (find-site site) route-specs))

(defmethod add-routes ((site site) route-specs)
  (setf *routes* nil)
  (routes:reset-mapper (site-route-map site))
  (mapcar #'(lambda (route-spec)
              (if (equal :include (first route-spec)) ;; route-spec is of form (:include *urls1* :name-prefix foo :template "/foo")
                  (mapcar #'(lambda (route)
                              (push route *routes*)
                              (routes:connect (site-route-map site)
                                              route))
                          (apply 'include (symbol-value (second route-spec)) (cddr route-spec)))
                  (routes:connect (site-route-map site)
                                  (first (push (apply #'route route-spec) *routes*)))))
          route-specs))
