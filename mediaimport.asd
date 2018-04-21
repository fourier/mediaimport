;;;; mediaimport.asd

(asdf:defsystem #:mediaimport
  :description "Import media files form external devices or directories"
  :author "Alexey Veretennikov <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:cl-fad
               #:cl-ppcre
               #:ironclad
               #:ppath
               #:split-sequence
               #:zpb-exif)
  :serial t
  :components ((:module "src"
                :serial t
                :components
                (#+lispworks(:file "settings")
                  (:file "version")
                  (:file "utils")
                  (:file "strings")
                  (:file "datetime")
                  (:file "renamer")
                  #+lispworks(:file "ui"))))
  :in-order-to ((test-op (test-op mediaimport-test))))
  

