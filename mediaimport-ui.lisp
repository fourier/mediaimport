;;;; mediaimport.lisp
;; 
;; To run, execute (mediaimport-ui:main)
;;
(defpackage #:mediaimport-ui
  (:use #:cl #:capi #:mediaimport)
  (:add-use-defaults t))

(in-package #:mediaimport-ui)
(annot:enable-annot-syntax)

(defvar *main-window* nil
  "Main window instance")
  

(define-interface main-window ()
  ((candidates :initform nil)
   (duplicates :initform nil))
  (:panes
   (input-directory-field text-input-pane)
   (input-button push-button :text "Choose Input directory..." :callback #'on-browse-button :data 'input)
   (output-directory-field text-input-pane)
   (output-button push-button :text "Choose Output directory..." :callback #'on-browse-button :data 'output)
   (recursive-checkbox check-button :text "Search in subdirectories")
   (exif-checkbox check-button :text "Use EXIF for JPG")
   (input-ext text-input-pane :title "Comma-separated list of extension[s], like \"jpg,png\"" :text "jpg")
   (output-ext text-input-pane :title "Output extension" :visible-max-width 40)
   (collect-button push-button :text "Collect data" :callback #'on-collect-button)
   (proposal-table multi-column-list-panel
      :visible-min-width 600
      :visible-min-height 200
      :callback-type :item-interface
      :column-function 'file-candidate-to-row
      :color-function 'color-file-candidate
      :action-callback 'edit-candidate-callback
      :columns '((:title "From" 
                  :adjust :left 
                  :visible-min-width (character 45))
                 (:title "To" 
                  :adjust :left 
                  :visible-min-width (character 45))
                 (:title "Comments" 
                  :adjust :left 
                  :visible-min-width (character 45))))
   (copy-button push-button :text "Copy..."))
   (:layouts
    (input-output-layout grid-layout '(input-button input-directory-field
                                       output-button output-directory-field)
                         :columns 2 :rows 2
                         :x-adjust '(:right :left)
                         :y-adjust '(:center :center))
    (extensions-layout grid-layout '(recursive-checkbox input-ext
                                     exif-checkbox output-ext)
                       :columns 2 :rows 2
                       :x-adjust '(:left :right)
                       :y-adjust '(:center :center))
    
    (main-layout column-layout '(input-output-layout
                                 extensions-layout
                                 collect-button
                                 proposal-table 
                                 copy-button) :adjust :center))
    
  (:default-initargs :title "Media Import"
   :visible-min-width 800
   :layout 'main-layout))

(defmethod initialize-instance :after ((self main-window) &key &allow-other-keys))

(defmethod set-candidates ((self main-window) new-candidates)
  (with-slots (candidates) self
    (setf candidates new-candidates)
    (notify-candidates-updated self)))

(defmethod notify-candidates-updated ((self main-window))
  (with-slots (candidates duplicates) self
    (if (not candidates)
        (setf duplicates nil)
        (setf duplicates (make-instance 'duplicate-finder
                                        :items candidates
                                        :key #'file-candidate-target)))))

  


(defun on-browse-button (data self)
  (with-slots (input-directory-field output-directory-field) self
    (let ((field nil)
          (message nil))
      (cond ((eql data 'input)
             (setf field input-directory-field
                   message "Import from"))
            ((eql data 'output)
             (setf field output-directory-field
                   message "Export to")))
      (multiple-value-bind (dir result) (prompt-for-directory message)
        (when result
          (setf (capi:text-input-pane-text field) (namestring dir)))))))


(defun on-collect-button (data self)
  (declare (ignore data))
  (with-slots (proposal-table
               input-directory-field
               output-directory-field
               input-ext
               output-ext
               recursive-checkbox
               exif-checkbox) self
      (let ((source-path (text-input-pane-text input-directory-field))
            (dest-path (text-input-pane-text output-directory-field)))
        (when (and (> (length source-path) 0) (> (length dest-path) 0))
          (let* ((extensions (text-input-pane-text input-ext))
                 (new-extension (text-input-pane-text output-ext))
                 (r (make-instance 'renamer
                                   :source-path source-path
                                   :destination-path dest-path
                                   :extensions extensions
                                   :new-extension new-extension
                                   :use-exif (button-selected exif-checkbox)))
                 (candidates (create-list-of-candidates r
                                                        :recursive (button-selected recursive-checkbox))))
            (setf (collection-items proposal-table)
                  candidates))))))


(defun file-candidate-to-row (cand)
  (list (file-candidate-source cand)
        (file-candidate-target cand)
        (if (fad:file-exists-p
             (file-candidate-target cand))
             "File already exists" "")))

(defun color-file-candidate (lp candidate state)
  (declare (ignore lp))
  (when (eq state :normal)
    (if (fad:file-exists-p
         (file-candidate-target candidate))
        :red
        nil)))

(defun edit-candidate-callback (item self)
  (with-slots (proposal-table) self
    (let ((message 
           (with-output-to-string (s)
             (format s "Rename ~a" (namestring (file-candidate-source item))))))
      (multiple-value-bind (fname result) 
          (prompt-for-string message :text (namestring (file-candidate-target item)))
        (when (and result
                   (not (equal fname (file-candidate-target item))))
          (setf (file-candidate-target item) fname)
          (redisplay-collection-item proposal-table item))))))


@export
(defun main ()
  (init)
  (setf *main-window* (make-instance 'main-window))
  (display *main-window*))
