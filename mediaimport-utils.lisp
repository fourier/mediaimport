;;;; mediaimport.lisp
(defpackage #:mediaimport-utils
  (:use #:cl #:cl-annot.class))

(in-package #:mediaimport-utils)
(annot:enable-annot-syntax)

@export
(defun interleave (list1 list2)
  "Interleaves 2 lists.
Example:
=> (interleave '(1 2 3) '(-1 -2 -3))
(1 -1 2 -2 3 -3)"
  (let ((parity t)
        (result (copy-list list1)))
    (merge 'list result list2
           (lambda (x y)
             (declare (ignore x) (ignore y))
             (setf parity (not parity))))
    result))



@export
(defclass duplicate-finder () ((items :initarg :items)
                               (key :initarg :key :initform #'identity)
                               (nonuniques-table :initform
                                                 (make-hash-table :test #'equal))))
                               

(defmethod initialize-instance :after ((self duplicate-finder) &key)
  (with-slots (items key nonuniques-table) self
    (mapc (lambda (x)
            (let* ((arg (funcall key x))
                   (ht-value (gethash arg nonuniques-table)))
              (if (not ht-value)
                  (setf (gethash arg nonuniques-table) 1)
                  (incf (gethash arg nonuniques-table)))))
          items)))
   

@export
(defmethod is-duplicate ((self duplicate-finder) arg)
  (with-slots (nonuniques-table) self
    (multiple-value-bind (value result)
        (gethash arg nonuniques-table)
      (values (if (not result) nil (> value 1)) result))))
