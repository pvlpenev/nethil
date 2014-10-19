;;;; nethil.asd

(asdf:defsystem #:nethil
  :serial t
  :description "very small webframework based on clack"
  :author "Pavel Penev <pvl.penev@gmail.com>"
  :license "MIT"
  :depends-on (#:clack
               #:routes)
  :pathname "src"
  :components ((:file "packages")
               (:file "nethil")))

