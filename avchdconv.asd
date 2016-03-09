;;;; avchdconv.asd

(asdf:defsystem #:avchdconv
  :description "Describe avchdconv here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:cl-fad
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "avchdconv")))

