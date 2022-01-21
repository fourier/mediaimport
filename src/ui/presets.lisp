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
  (:export
   preset
   preset-name
   preset-checkboxes
   preset-edits
   preset-radioboxes
   preset-load
   list-presets
   load-default-preset
   create-default-preset
   create-preset
   remove-preset
   load-presets))

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
   (edits :initarg :edits :initform nil :reader preset-edits
          :documentation
          "A hash table where the key is a symbol and a value is a string, key being edit unique identifier")
   (checkboxes :initarg :checkboxes :initform nil :reader preset-checkboxes
               :documentation "A list of symbols, where every symbol is an identifier of a crossed checkbox item")
   (radioboxes :initarg :radioboxes :initform nil :reader preset-radioboxes
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
    ;; Handle the edits separately. It could be
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

(defmethod preset-load ((name string) (settings settings) &optional (base-path *presets-path*))
  "Populate the preset from settings, or leave defaults.
Return nil if not found in registry"
  (let* ((md5name (md5string name))
         (preset-path (create-path base-path md5name))
         (preset (make-instance 'preset :name name :base-path base-path)))
    (when-let (realname (get-value settings (create-path preset-path *presets-name-path*)))
      (when (string= realname name) ;; sanity check
        ;; now let's load fields
        ;; start with edits
        (when-let (edits (get-value settings (create-path preset-path *presets-edits-prefix*)))
          (setf (slot-value preset 'edits)
                (alist-hash-table edits)))
        ;; now checkboxes
        (when-let (checkboxes (get-value settings (create-path preset-path *presets-checkboxes-prefix*)))
          (setf (slot-value preset 'checkboxes) checkboxes))
        ;; and radio boxes
        (when-let (radioboxes (get-value settings (create-path preset-path *presets-radioboxes-prefix*)))
          (setf (slot-value preset 'radioboxes) radioboxes))
        ;; return value
        preset))))

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
               checkboxes)
    ;; and radio boxes
    (set-value settings (create-path registry-path *presets-radioboxes-prefix*)
               radioboxes)
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

(defmethod load-default-preset ((settings settings))
  "Load the default preset or NIL if not found"
  (preset-load *presets-default-preset-name* settings *presets-default-path*))

(defmethod create-default-preset ((settings settings) edits checkboxes radioboxes)
  "Creates and saves the default preset with values provided"
  (let ((default
         (make-instance 'preset :name *presets-default-preset-name*
                        :edits edits
                        :checkboxes checkboxes
                        :radioboxes radioboxes
                        :base-path *presets-default-path*)))
    (preset-save default settings)))

(defmethod create-preset ((settings settings) name edits checkboxes radioboxes)
  "Creates and saves the preset with then name and values provided"
  (let ((new-preset
         (make-instance 'preset :name name
                        :edits edits
                        :checkboxes checkboxes
                        :radioboxes radioboxes)))
    (preset-save new-preset settings)))

(defmethod load-presets ((settings settings))
  "Return the list of presets, without default"
  (loop for name in (list-presets settings)
        for preset = (preset-load name settings)
        when preset
        collect preset))

(defmethod remove-preset ((settings settings) name)
  "Remove preset from the list"
  (let ((registry-path
         (concatenate 'string (create-path *presets-path* (md5string name)) "/"))
        (presets-list (list-presets settings)))
    ;; FIXME: This does not remove the registry item
    (set-value settings registry-path nil)
    (set-value settings *presets-list-path*
               (remove-if (curry #'string= name) presets-list))))


