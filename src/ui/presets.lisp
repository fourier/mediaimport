;;;; presets.lisp
;; 

(in-package #:mediaimport.ui)

(defparameter *default-preset-name* "<<Default>>"
  "Hard-coded name for the default preset")

(defclass preset ()
  ((name :initarg :name :initform *default-preset-name* :reader preset-name)
   (edits :initarg :edits :initform nil)
   (checkboxes :initarg :checkboxes :initform nil)
   (radioboxes :initarg :radioboxes :initform nil)
   (registry-path :initarg :registry-path :initform "Presets"))
  (:documentation "A collection of values for the form. Arguments to constructor
should all be lists of symbols"))


(defmethod initialize-instance :after ((self preset) &key &allow-other-keys)
  "Constructor for the preset class"
  (macrolet ((populate-table (table-name)
               `(loop with table = (make-hash-table)
                      initially (setf ,table-name table)
                      for v in ,table-name
                      do (setf (gethash v table) nil))))
    (with-slots (edits checkboxes radioboxes) self
      (populate-table edits)
      (populate-table checkboxes)
      (populate-table radioboxes))))


(defmethod preset-load ((preset preset) (settings settings))
  "Populate the preset from settings, or leave defaults"
  (with-slots (edits checkboxes radioboxes registry-path) preset
    ;; form the path to preset in form "Presets/PRESET_MD5"
    (let ((base-path
           (string-append registry-path "/" (md5string (preset-name preset)) "/")))
      (multiple-value-bind (val found) (get-value settings (string-append base-path "name"))
        (when found ; yahoo! we got something saved, at least name
          (setf (slot-value preset 'name) val)
          ;; now let's load fields
          ;; start with edits
          (loop for k being each hash-key of edits
                for path = (string-append base-path (symbol-name k))
                for value = (get-value settings path "")
                do (setf (gethash k edits) value))
          ;; now checkboxes
          (loop for k being each hash-key of checkboxes
                for path = (string-append base-path (symbol-name k))
                for value = (get-value settings path nil)
                do (setf (gethash k checkboxes) value))
          ;; and radio boxes
          (loop for k being each hash-key of radioboxes
                for path = (string-append base-path (symbol-name k))
                for (value result) = (multiple-value-list
                                      (get-value settings path))
                if (not result)
                do (remhash k radioboxes)
                else
                do (setf (gethash k edits) value)))))))


(defmethod preset-save ((preset preset) (settings settings))
  "Save the preset to settings"
  ;; form the path to preset in form "Presets/PRESET_MD5"
  (let ((base-path (string-append "Presets/" (md5string (preset-name preset)) "/")))
    ;; save the name
    (let ((path (string-append base-path "name")))
      (set-value settings path (slot-value preset 'name)))
    ;; now let's save fields      
    (with-slots (edits checkboxes radioboxes) preset
      ;; start with edits
      (loop for k being each hash-key of edits using (hash-value v)
            for path = (string-append base-path (symbol-name k))
            do (set-value settings path v))
      ;; now checkboxes
      (loop for k being each hash-key of checkboxes using (hash-value v)
            for path = (string-append base-path (symbol-name k))
            do (set-value settings path v))
      ;; and radio boxes
      (loop for k being each hash-key of radioboxes using (hash-value v)
            for path = (string-append base-path (symbol-name k))
            do (set-value settings path v)))))

  
;;; (defmethod list-presets ((settings settings))
;;;   )
  
