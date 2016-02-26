;;;; nethil.asd

(asdf:defsystem #:nethil
  :serial t
  :description "A very small webframework based on clack"
  :author "Pavel Penev <pvl.penev@gmail.com>"
  :license "MIT"
  :depends-on (#:clack
               #:routes)
  :pathname "src"
  :components ((:file "packages")
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
