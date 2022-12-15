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
               #:zpb-exif
               #+lispworks #:lw-settings)
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
                 #+lispworks
                 (:module "ui"
                  :serial t
                  :components
                  ((:file "presets")
                   (:file "interfaces")
                   (:file "functions")
                   (:file "callbacks")))
                 #+lispworks (:file "mediaimport"))))
  :in-order-to ((test-op (test-op mediaimport-test))))
  

