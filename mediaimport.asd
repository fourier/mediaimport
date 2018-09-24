;;;; mediaimport.asd

(asdf:defsystem #:mediaimport
  :description "Import media files form external devices or directories"
  :author "Alexey Veretennikov <alexey.veretennikov@gmail.com>"
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
                ((:file "version")
                 #+lispworks(:file "settings")
                 (:file "utils")
                 (:file "strings")
                 (:file "datetime")
                 (:file "renamer")
                 #+lispworks(:file "ui"))))
  :in-order-to ((test-op (test-op mediaimport-test))))
  

