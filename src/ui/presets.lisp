;;;; presets.lisp
;; 

(defpackage #:mediaimport.ui.presets
  (:documentation "User interface definitions for MediaImport application")
  (:use #:cl #:alexandria
        #:mediaimport.utils #:mediaimport.settings)
  ;; these names should be from alexandria rather than lispworks
  (:shadowing-import-from #:alexandria
   if-let removef when-let* appendf copy-file with-unique-names nconcf when-let)
  (:add-use-defaults t)
  (:export preset))

(in-package #:mediaimport.ui.presets)

;;----------------------------------------------------------------------------
;; Constants and defaults
;;----------------------------------------------------------------------------

(defparameter *presets-default-preset-name* "<<Default>>"
  "Hard-coded name for the default preset")

(defparameter *presets-path* "Presets/Presets"
  "Registry path where presets are stored")

(defparameter *presets-default-path* "Presets"
  "Registry path for default preset")

(defparameter *presets-list-path* "Presets/list"
  "Registry path for the list of presets")

(defparameter *presets-name-path* "name"
  "Relative path to the preset name")

(defparameter *presets-edits-prefix* "Edits"
  "Prefix (after preset path) of the field for storing Edits for the preset")

(defparameter *presets-checkboxes-prefix* "Checkboxes"
  "Prefix (after preset path) of the field for storing checkboxes for the preset")

(defparameter *presets-radioboxes-prefix* "Radioboxes"
  "Prefix (after preset path) of the field for storing radioboxes for the preset")


;;----------------------------------------------------------------------------
;; Preset class
;;----------------------------------------------------------------------------

(defclass preset ()
  ((name :initarg :name :initform *default-preset-name* :reader preset-name
         :documentation "User-readable preset name. Will be used in UI elements - menus etc")
   (edits :initarg :edits :initform nil
          :documentation
          "A hash table where the key is a symbol and a value is a string, key being edit unique identifier")
   (checkboxes :initarg :checkboxes :initform nil
               :documentation "A list of symbols, where every symbol is an identifier of a crossed checkbox item")
   (radioboxes :initarg :radioboxes :initform nil
               :documentation "A list of symbols, where every symbol is an identifier of a active radiobox item")
   (registry-base-path :initarg :base-path :initform *presets-path*
                       :documentation "A path in registry where the preset will be created by appending its hashed name")
   (registry-path :reader preset-registry-path
                  :documentation "An unique path to this preset in registry"))
  (:documentation "A named collection of the values for the form.
It assumes none of edits have equal symbol names, same for the checkboxes and radioboxes."))

(defun create-path (base-path &rest paths)
  "Create /-separated path from base path and rest compontents.
Based on ppath:join"
  (unless base-path
    (setf base-path ""))
  (cond ((null paths) ;; finalizing recursion clause
         base-path)
        ((starts-with #\/ (car paths))
         ;; a component is an absolute path, discard
         ;; all previous components to the left
         (apply #'create-path (car paths) (cdr paths)))
        ((or (emptyp base-path) (ends-with #\/ base-path))
         ;; just concat and continue to join
         (apply #'create-path (concatenate 'string base-path (car paths)) (cdr paths)))
        (t ;; have to add "/" in between
         (apply #'create-path (concatenate 'string base-path "/" (car paths)) (cdr paths)))))


(defmethod initialize-instance :after ((self preset) &key &allow-other-keys)
  "Constructor for the preset class"
  (with-slots (edits registry-path registry-base-path) self
    ;; first handle the edits. It could be
    ;; - nil
    ;; - a list of pairs (symbol . string)
    ;; - a hash table
    (setf edits
          (typecase edits
            (null (make-hash-table))
            (list (alist-hash-table edits))
            (hash-table edits)))
    ;; form the path to preset in form "Presets/Presets/PRESET_MD5/"  
    (setf registry-path
          (create-path registry-base-path (md5string (preset-name self))))))

#|
(defmethod preset-load ((name string) (settings settings) &optional (base-path *presets-path*))
  "Populate the preset from settings, or leave defaults.
Return nil if not found in registry"
  (let ((md5name (md5string name))
        (preset-path (create-path base-path md5name))
        (preset (make-instance 'preset :name name :base-path base-path))
    (when-let (realname (get-value settings (create-path preset-path *presets-name-path*)))
      (when (string= realname name) ;; sanity check
        ;; start with edits
          (when-let (edits (get-value settings ((create-path registry-path *presets-edits-prefix*))))
            (let ((ht (make-hash-table)))
              (setf (slot-value 'edits preset)
                    (loop for 
  (with-slots (edits checkboxes radioboxes registry-path) preset
    (multiple-value-bind (val found) (get-value settings (create-path registry-path *presets-name-path*))
      (when found ; yahoo! we got something saved, at least name
        (setf (slot-value preset 'name) val)
        ;; now let's load fields
        ;; start with edits
        (loop for k being each hash-key of edits
              for path = (create-path registry-path (symbol-name k))
              for value = (get-value settings path "")
              do (setf (gethash k edits) value))
        ;; now checkboxes
        (loop for k being each hash-key of checkboxes
              for path = (create-path registry-path (symbol-name k))
              for value = (get-value settings path nil)
              do (setf (gethash k checkboxes) value))
        ;; and radio boxes
        (loop for k being each hash-key of radioboxes
              for path = (create-path registry-path (symbol-name k))
              for (value result) = (multiple-value-list
                                    (get-value settings path))
              if (not result)
              do (remhash k radioboxes)
              else
              do (setf (gethash k edits) value)))
      ;; return value
      found)))
|#

(defmethod preset-save ((preset preset) (settings settings))
  "Save the preset to settings"
  (with-slots (name edits checkboxes radioboxes registry-path registry-base-path)
      preset
    ;; save the name
    (let ((path (create-path registry-path *presets-name-path*)))
      (set-value settings path name))
    ;; now let's save fields      
    ;; start with edits
    (set-value settings (create-path registry-path *presets-edits-prefix*)
               (hash-table-alist edits))
    ;; now checkboxes
    (set-value settings (create-path registry-path *presets-checkboxes-prefix*)
               (mapcar #'symbol-name checkboxes))
    ;; and radio boxes
    (set-value settings (create-path registry-path *presets-radioboxes-prefix*)
               (mapcar #'symbol-name radioboxes))
    ;; finally update the list of presets for non-default preset
    (when (string= *presets-path* registry-base-path)
      (let ((presets-list (list-presets settings)))
        (unless (member name presets-list :test #'string=)
          (set-value settings *presets-list-path*
                     (nreverse (cons name (nreverse presets-list)))))))))

;;----------------------------------------------------------------------------
;; Global functions related to presets
;;----------------------------------------------------------------------------

(defmethod list-presets ((settings settings))
  "Return the list of current preset names from settings"
  (get-value settings *presets-list-path*))

(defmethod save-presets-list ((settings settings) presets)
  "Save the list of names or presets, extracting their names in this case, in settings"
  (let ((names 
         (mapcar (lambda (p)
                   (etypecase p
                     (preset (preset-name p))
                     (string p)))
                 presets)))
    (set-value settings *presets-list-path* names)))

(defmethod load-default-preset ((settings settings))
  "Load the default preset or NIL if not found"
  (let ((default (make-instance 'preset :registry-path *presets-default-path*)))
    (when (preset-load default settings)
      default)))


(defmethod load-presets ((settings settings))
  "Return the list of presets, without default"
  (mapcar (lambda (name)
            (let ((preset (make-instance 'preset :name name)))
              (preset-load preset settings)
              preset))
          (list-presets settings)))
