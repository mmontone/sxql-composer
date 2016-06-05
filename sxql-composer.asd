(asdf:defsystem #:sxql-composer
  :description "Build and compose SXQL queries dynamically"
  :version "0.1"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license "MIT"
  :depends-on (#:sxql)
  :components ((:file "package")
               (:file "sxql-composer"))
  :serial t)
