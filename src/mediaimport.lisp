;;;; ui.lisp
;; 
;; To run, execute (mediaimport.ui:main)
;;
(defpackage #:mediaimport
  (:documentation "MediaImport application")
  (:use #:cl #:capi
        #:mediaimport.ui)
  (:add-use-defaults t)
  (:export main))

(in-package #:mediaimport)


;;----------------------------------------------------------------------------
;; The application entry point
;;----------------------------------------------------------------------------

(defun main ()
  "Platform-dependent initialization"
  #+sbcl
  (setf SB-ALIEN::*DEFAULT-C-STRING-EXTERNAL-FORMAT* :UTF-8)
  #+lispworks
  (lw:set-default-character-element-type 'lw:bmp-char)
  #+cocoa
  (let ((application (make-instance 'cocoa-application-interface)))
    (set-application-interface application)
    (let ((main-window (make-instance 'main-window
                                      :application-interface application)))
      (setf (main-window application) main-window)
      (display main-window)))
  #+win32
  (let ((interface (make-instance 'main-window :application-interface nil)))
      (display interface)))
