;;;; mediaimport.asd

(asdf:defsystem #:mediaimport
  :description "Import media files form external devices or directories"
  :author "Alexey Veretennikov <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:cl-fad
               #:cl-ppcre
               #:ironclad
               #:cl-annot
               #:split-sequence
               #:zpb-exif)
  :serial t
  :components (#+lispworks(:file "settings")
               (:file "version")
               (:file "utils")
               (:file "strings")
               (:file "datetime")
               (:file "renamer")
               #+lispworks(:file "ui")))

