#| worker.lisp

  This file is a part of mediaimport project.

  (C) COPYRIGHT Alexey Veretennikov<alexey.veretennikov@protonmail.com>, 2022
|#
(in-package :cl-user)

(defpackage :mediaimport.worker
  (:documentation "Worker process package")
  (:use :cl)
  (:add-use-defaults t)
  (:export
   worker-stop
   worker-running-p
   worker-shedule))


(in-package mediaimport.worker)

(defclass worker ()
  ((process :documentation "Worker thread/process"
            :reader worker-process
            :initform nil)
   (name :initarg name
         :initform "Worker"
         :documentation "Process name")
   (event-action
    :initarg event-action-callback
    :initform #'(lambda (arg) (declare (ignore arg)) (values))
    :documentation "Callback (1 arg) to be called when event appears in a queue")))

(defmethod initialize-instance :after ((self worker) &key)
  (with-slots (process name) self
    (setf process
          (mp:process-run-function
           name
           '(:mailbox t :internal-server t)
           #'worker-main self))))
    

(defmethod worker-main ((self worker))
  "Main worker thread. Wait for the messages from the mailbox"
  (with-slots (process event-action) self
    (let ((mb (mp:process-mailbox process)))
      (loop for event = (mp:mailbox-read mb)
            if event
            do
            (funcall event-action event)
            ;; on nil exit thread
            else
            return nil))))

(defmethod worker-stop ((self worker))
  "Stop the worker process"
  (when (worker-running-p self)
    (mp:process-send (worker-process self) nil)))

(defmethod worker-running-p ((self worker))
  "Return T if the logger process is initialized and accepting messages"
  (and (not (null (worker-process self)))
       (eql (mp:process-state (worker-process self)) :active)))
   
