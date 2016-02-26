;;;; nethil.lisp

(in-package #:nethil)

;;; "nethil" goes here. Hacks and glory await!

(defun start (app &key (port 8000))
  (let ((server (make-instance 'server :port port)))
    (push app (server-apps server))
    (push server *servers*)
    (mount-all-routes)
    (setf (server-clack-handler server) (clack:clackup server :port port))))

(defun stop-all ()
  (loop for server in *servers*
     do (clack:stop (server-clack-handler server)))
  (setf *servers* nil))
