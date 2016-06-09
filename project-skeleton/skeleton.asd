
(asdf:defsystem <% @var name %>
  :version "0.1"
  :author "<% @var author %>"
  :license "<% @var license %>"
  :description "<% @var description %>" 
  :depends-on (:nethil :djula)
  :components ((:module "src"                        
                        :components
                        ((:file "config")
                         (:file "urls" :depends-on ("config"))
                         (:file "<% @var name %>" :depends-on ("urls"))))))




