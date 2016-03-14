;;;; mediaimport.asd

(asdf:defsystem #:mediaimport
  :description "Import media files form external devices or directories"
  :author "Alexey Veretennikov <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:cl-fad
               #:cl-ppcre
               #:ironclad)
  :serial t
  :components ((:file "package")
               (:file "mediaimport")))

