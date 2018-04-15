;;;; settings.lisp
;; 
(defpackage #:mediaimport.settings
  (:documentation "MediaImport application settings")
  (:use #:cl #:alexandria)
  ;; these names should be from alexandria rather than lispworks
  (:shadowing-import-from #:alexandria if-let removef when-let* appendf copy-file with-unique-names nconcf when-let)
  (:nicknames #:settings)
  (:add-use-defaults t)
  (:export
   settings
   company
   application-name
   application-version
   product
   get-value
   set-value))

(in-package #:mediaimport.settings)


(defclass settings ()
  ((company :reader company
            :initarg :company
            :initform "com.github.fourier"
            :documentation "Company name")
   (name :reader application-name
         :initarg :application-name
         :initform "MediaImport"
         :documentation "Application name")
   (version :reader application-version
            :initarg :application-version
            :initform "1.0"
            :documentation "Application version")
   (company-symbol :reader product
    :documentation "A symbol produced from the company name")
   (settings-path
    :documentation "A path to use to access settings"))
  (:documentation "Settings class provides application-specific persistence settings"))

(defmethod print-object ((self settings) out)
  "Print overload for the SETTINGS class"
  (print-unreadable-object (self out :type t)
    (format out "~%   Application name: ~s" (application-name self))
    (format out "~%   Company name: ~s" (company self))
    (format out "~%   Application version: ~s" (application-version self))))


(defmethod initialize-instance :after ((self settings) &key)
  "Constructor for SETTINGS class"
  (with-slots (company name version company-symbol settings-path) self
    (setf company-symbol (intern (string-upcase company)))
    (setf settings-path
          (setf (sys:product-registry-path company-symbol) (list name version)))))


(defmethod get-value ((self settings) key &optional fallback-value)
  "Get the value identified by KEY from the storage SELF.
If FALLBACK-VALUE specified, use this if not found (and update the storage)"
  (with-slots (company-symbol settings-path) self
    (multiple-value-bind (value result)
        (user-preference settings-path key :product company-symbol)
      (cond ((and result value) (values value result))
            (fallback-value
             (progn
               (setf (user-preference settings-path key :product company-symbol) fallback-value)
               (values (user-preference settings-path key :product company-symbol) t)))
            (t (values nil nil))))))


(defmethod set-value ((self settings) key value)
  "Set and save the VALUE identified by the KEY in storage SELF."
  (with-slots (company-symbol settings-path) self
    (setf (user-preference settings-path key :product company-symbol) value)
    value))



