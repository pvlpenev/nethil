;;;; nethil.asd

(asdf:defsystem #:nethil
  :serial t
  :description "A very small webframework based on clack"
  :author "Pavel Penev <pvl.penev@gmail.com>"
  :license "MIT"
  :version "0.2"
  :depends-on (#:alexandria
               #:clack
               #:routes)
  :pathname "src"
  :components ((:file "packages")
               (:file "globals")
               (:file "app")
               (:file "routes")
               (:file "nethil")))

(asdf:defsystem #:nethil-demo
  :serial t 
  :description "nethil demo app"
  :author "Pavel Penev <pvl.penev@gmail.com>"
  :licence "MIT"
  :depends-on (#:nethil)
  :pathname "demo"
  :components ((:file "packages")
               (:file "nethil-demo")))
