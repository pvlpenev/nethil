;;;;; project.lisp

(cl:defpackage :nethil.project
  (:use :cl)
  (:export #:project))

(in-package :nethil.project)

(defvar *skeleton-directory*
  (asdf:system-relative-pathname :nethil #p"project-skeleton/"))

(defun project (path &rest params &key name description author email license &allow-other-keys)
  (declare (ignore name description author email license))
  (let ((cl-project:*skeleton-directory* *skeleton-directory*))
    (apply #'cl-project:make-project path params)))

;; TODO figure out how to automatically add apps
