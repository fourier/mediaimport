;;;; mediaimport.lisp
;; 
;; To run, execute (mediaimport-ui:main)
;;
(defpackage #:mediaimport-ui
  (:use #:cl #:capi #:mediaimport)
  (:add-use-defaults t))

(in-package #:mediaimport-ui)
(annot:enable-annot-syntax)
;;(declaim (optimize (debug 3)))

(defvar *main-window* nil
  "Main window instance")

(define-interface main-window ()
  ()
  (:panes
   (input-directory-field text-input-pane)
   (input-button push-button :text "Choose Input directory..." :callback #'on-browse-button :data 'input)
   (output-directory-field text-input-pane)
   (output-button push-button :text "Choose Output directory..." :callback #'on-browse-button :data 'output)
   (recursive-checkbox check-button :text "Search in subdirectories")
   (input-ext text-input-pane :title "Extension" :text "*.*")
   (output-ext text-input-pane :title "Output extension")
   (collect-button push-button :text "Collect data" :callback #'on-collect-button)
   (proposal-table multi-column-list-panel
      :visible-min-width 600
      :visible-min-height 200
;      :visible-min-height :text-height
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
    (extensions-layout row-layout '(recursive-checkbox input-ext output-ext)
                       :y-adjust '(:center))
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
  (with-slots (proposal-table input-directory-field output-directory-field recursive-checkbox) self
      (let ((source-path (text-input-pane-text input-directory-field))
            (dest-path (text-input-pane-text output-directory-field)))
        (when (and (> (length source-path) 0) (> (length dest-path) 0))
          (let* ((r (make-instance 'renamer
                                   :source-path source-path
                                   :destination-path dest-path
                                   :extensions "jpg" :new-extension "png"))
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

(defun edit-candidate-callback (data self)
  (with-slots (proposal-table) self
    (let ((message 
           (with-output-to-string (s)
             (format s "Rename ~a" (namestring (file-candidate-source data))))))
      (multiple-value-bind (fname result) 
          (prompt-for-string message :text (namestring (file-candidate-target data)))
        (when (and result
                   (not (equal fname (file-candidate-target data))))
          (setf (file-candidate-target data) fname)
          (redisplay-collection-item proposal-table data))))))


@export
(defun main ()
  (init)
  (setf *main-window* (make-instance 'main-window))
  (display *main-window*))
