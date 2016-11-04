(asdf:defsystem "web-util"
  :description "some utility functions for web programming"
  :version "0.0.1"
  :author "Travis"
  :licence "AGPL-3.0"
  :depends-on ("hunchentoot"
               "cl-ppcre"
               "spinneret"
               "cl-dbi"
               "quri")
  :components ((:file "package")
               (:file "src/web-util" :depends-on ("package"))))
