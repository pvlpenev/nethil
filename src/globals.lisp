;;;; globals.lisp

(in-package #:nethil)

(defvar *sites* nil "List of sites")
(defvar *site* nil "Current site")
(defvar *apps* nil "List of defined apps")
(defvar *app* nil "Current app")
(defvar *routes* nil "alist of defined routes")
(defvar *running-apps* nil "List of apps mounted in servers")
(defvar *env* nil "Current clack environment")
(defvar *bindings* nil "Current route bindings")
