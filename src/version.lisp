;;;; version.lisp
;; 
;; MediaImport version
;;
(defpackage #:mediaimport.version
  (:documentation "Version definition for MediaImport application")
  (:use #:cl)
  (:export version-string))

(in-package #:mediaimport.version)

(defparameter +version+ "v0.5")

(defun version-string ()
  +version+)
