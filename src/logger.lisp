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
   logger-start
   logger-stop
   logger-initialized-p
   logger-info))


(in-package mediaimport.logger)

(defparameter *logger-process* nil
  "Thread for logging")

(defun logger-info (message &rest args)
  "Send a message to the logger with format arguments"
  (when (logger-initialized-p)
    (let ((msg (apply #'format (append (list nil message) args))))
      (mp:process-send *logger-process* msg)))
  (values))

(defun default-output (message)
  "Default logger output function - standard output"
  (format t "~a" message))

(defun logger-start (&optional (output-function #'default-output))
  "[Re]start the logger process.
OUTPUT-FUNCTION function of the one (string) argument which will
be called by the logger thread to do the output of its argument"
  ;; Stop running thread
  (when (logger-initialized-p)
    (logger-stop))
  ;; Create a new instance
  (setf *logger-process*
        (mp:process-run-function "Logger thread"
                                 '(:mailbox t :internal-server t)
                                 #'logger-main output-function)))

(defun logger-main (output-function)
  "Main logger thread. Wait for the messages from the mailbox"
  (let ((mb (mp:process-mailbox *logger-process*)))
    (loop for event = (mp:mailbox-read mb)
        if event
        do
        (funcall output-function event)
        ;; on nil exit thread
        else
        return nil)))

(defun logger-stop ()
  "Stop the logger process"
  (when (logger-initialized-p)
    (mp:process-send *logger-process* nil)))

(defun logger-initialized-p ()
  "Return T if the logger process is initialized and accepting messages"
  (and (not (null *logger-process*))
       (eql (mp:process-state *logger-process*) :active)))
   
