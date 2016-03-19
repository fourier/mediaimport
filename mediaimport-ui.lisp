;;;; mediaimport.lisp
(defpackage #:mediaimport-ui
  (:use #:cl #:capi #:mediaimport)
  (:add-use-defaults t))

(in-package #:mediaimport-ui)
(annot:enable-annot-syntax)

(defvar *main-window* nil
  "Main window instance")

(define-interface main-window ()
  ()
  (:panes
   (input-label display-pane :text "Input directory")
   (input-directory-field text-input-pane)
   (input-button push-button :text "Browse" :callback #'on-browse-button :data 'input)
   (output-label display-pane :text "Output directory")   
   (output-directory-field text-input-pane)
   (output-button push-button :text "Browse" :callback #'on-browse-button :data 'output)
   (input-ext text-input-pane :title "Extension" :text "*.*")
   (output-ext text-input-pane :title "Output extension")
   (collect-button push-button :text "Collect data" :callback #'on-collect-button)
   (proposal-table multi-column-list-panel
      :visible-min-width 600
      :visible-min-height :text-height
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
    (input-output-layout grid-layout '(input-label input-directory-field input-button
                                       output-label output-directory-field output-button)
                         :columns 3 :rows 2
                         :x-adjust '(:right :left)
                         :y-adjust '(:center :center))
    (extensions-layout row-layout '(input-ext output-ext))
    (main-layout column-layout '(input-output-layout
                                 extensions-layout
                                 collect-button
                                 proposal-table 
                                 copy-button) :adjust :center))
    
  (:default-initargs :title "Media Import"
   :visible-min-width 800
   :layout 'main-layout))

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
  (with-slots (proposal-table input-directory-field output-directory-field) self
      (let ((source-path (text-input-pane-text input-directory-field))
            (dest-path (text-input-pane-text output-directory-field)))
        (when (and (> (length source-path) 0) (> (length dest-path) 0))
          (let* ((r (make-instance 'renamer
                                   :source-path source-path
                                   :destination-path dest-path
                                   :extensions "jpg" :new-extension "png"))
                 (candidates (create-list-of-candidates r)))
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

(defun edit-candidate-callback (data interface)
  (let ((current-path (file-candidate-target data)))
    (let ((message 
           (with-output-to-string (s)
             (format s "Rename ~a to" (namestring (file-candidate-source data))))))
      (prompt-for-string message :text (namestring (file-candidate-target data))))))

@export
(defun main ()
  (init)
  (setf *main-window* (make-instance 'main-window))
  (display *main-window*))
