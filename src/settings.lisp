;;;; settings.lisp
;; 
(defpackage #:mediaimport.settings
  (:documentation "MediaImport application settings")
  (:use #:cl)
  (:nicknames #:settings)
  (:import-from #:lw.settings get-value set-value product-name)
  (:import-from #:alexandria define-constant)
  (:add-use-defaults t)
  (:export
   make-app-settings
   product-name
   get-value
   set-value))

(in-package #:mediaimport.settings)

(define-constant +company+ "com.github.fourier" :test #'string=)
(define-constant +application-name+ "MediaImport" :test #'string=)

(defun make-app-settings ()
  (lw.settings:make-settings +application-name+ +company+ (mediaimport.version:version-string)))
