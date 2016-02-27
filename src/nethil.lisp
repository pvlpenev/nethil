;;;; nethil.lisp

(in-package #:nethil)

;;; "nethil" goes here. Hacks and glory await!

(defclass site (lack.component:lack-component)
  ((name :initarg :name :reader site-name)
   (hostname :initarg :hostname :reader site-hostname)
   (port :initarg :port :reader site-port)
   (route-map :initform (make-instance 'routes:mapper) :reader site-route-map)
   (apps :initform nil :accessor site-apps)
   (apps-spec :initform nil :accessor site-apps-spec)
   (handler :initform nil :accessor site-clack-handler))
  (:documentation "Class for a server instance."))

(defmethod lack.component:call ((site site) env)
  (let ((*env* env)
        (*site* site))
    (multiple-value-bind (route *bindings*)
        (routes:match (site-route-map site) (getf *env* :request-uri))
      (if route
          (let ((result (funcall (route-handler route))))
            (if (stringp result)
                (list 200 (list "text/html") (list result))
                result))
          '(404 (:content-type "text/plain") ("Not Found"))))))

(defmacro define-site (name &key (hostname "localhost") (port 8000))
  `(push (make-instance 'site :name ',name :hostname ,hostname :port ,port) *sites*))

(defun find-site-by-name (name)
  (find name *sites* :key #'site-name :test #'eql))

(defun find-site (hostname port)
  (find (list hostname port) *sites*
        :key (lambda (site)
               (list (site-hostname site)
                     (site-port site)))
        :test #'equal))

(defun mount-site-routes (site)
  (routes:reset-mapper (site-route-map site))
  (loop for app in (site-apps site)
       do (map-app-routes app (site-route-map site))))

(defun mount-site-apps (site)
  (setf (site-apps site) nil)
  (loop for app-spec in (site-apps-spec site)
     do (push (apply #'mount-app app-spec) (site-apps site))))

(defun %compose (site apps-spec)
  (let ((site (find-site-by-name site)))
    (setf (site-apps-spec site) apps-spec)
    (mount-site-apps site)
    (mount-site-routes site)
    site))

(defmacro compose (site &body apps-spec)
  `(progn
     (%compose ',site ',apps-spec)))

(defun mount-all-routes ()
  (loop for site in *sites*
     do (mount-site-apps site)
     do (mount-site-routes site)))

(defun start (site)
  (setf (site-clack-handler site)
        (clack:clackup site
                       :hostname (site-hostname site)
                       :port (site-port site))))

(defun stop (site)
  (clack:stop (site-clack-handler (find-site-by-name site))))

(defun stop-all ()
  (loop for site in *sites*
       do (clack:stop (site-clack-handler site))))
