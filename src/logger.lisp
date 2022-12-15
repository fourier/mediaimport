#| logger.lisp

  This file is a part of mediaimport project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@protonmail.com>, 2022
|#
(in-package :cl-user)

(defpackage :mediaimport.logger
  (:documentation "Logger package")
  (:use :cl)
  (:add-use-defaults t)
  (:export
   logger-init
   logger-initialized-p
   logger-info))


(in-package mediaimport.logger)

(defparameter *logger-process* nil
  "Thread for logging")

(defun logger-info (message &rest args)
  (let ((msg (apply #'format (append (list nil message) args))))
    (when (logger-initialized-p) (mp:process-send *logger-process* msg)))
  (values))

(defun default-output (message)
  (format t "~a" message))

(defun logger-init (&optional (output-function #'default-output))
  ;; Stop running thread
  (when *logger-process*
    (mp:process-stop *logger-process*))
  ;; Create a new instance
  (setf *logger-process*
        (mp:process-run-function "Logger thread" '(:mailbox t :internal-server t)  #'logger-main output-function)))

(defun logger-main (output-function)
  (loop for event = 
        (mp:mailbox-read
         (mp:process-mailbox *logger-process*))
        when event
        do
        (funcall output-function event)))


(defun logger-initialized-p ()
  (not (eq *logger-process* nil)))
   
