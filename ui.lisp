;;;; mediaimport.lisp
;; 
;; To run, execute (mediaimport.ui:main)
;;
(defpackage #:mediaimport.ui
  (:use #:cl #:capi #:mediaimport.utils #:mediaimport.renamer)
  (:add-use-defaults t))

(in-package #:mediaimport.ui)
(annot:enable-annot-syntax)

(defvar *main-window* nil
  "Main window instance")
  

(define-interface main-window ()
  ((duplicates :initform nil))
  (:panes
   (input-directory-field text-input-pane :callback #'on-collect-button)
   (input-button push-button :text "Choose Input directory..." :callback #'on-browse-button :data 'input )
   (output-directory-field text-input-pane :callback #'on-collect-button)
   (output-button push-button :text "Choose Output directory..." :callback #'on-browse-button :data 'output)
   (recursive-checkbox check-button :text "Search in subdirectories")
   (exif-checkbox check-button :text "Use EXIF for JPG")
   (input-ext text-input-pane :title "Comma-separated list of extension[s], like \"jpg,png\"" :text "jpg")
   (output-ext text-input-pane :title "Output extension" :visible-max-width 40)
   (prefix text-input-pane :title "Prefix (like \"Photo-\"" :text "Photo-")
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
   (copy-button push-button :text "Copy..." :callback #'on-copy-button)
   (progress-bar progress-bar))
   (:layouts
    (input-output-layout grid-layout '(input-button input-directory-field
                                       output-button output-directory-field)
                         :columns 2 :rows 2
                         :x-adjust '(:right :left)
                         :y-adjust '(:center :center))
    (extensions-layout grid-layout '(recursive-checkbox input-ext
                                     exif-checkbox output-ext prefix nil)
                       :columns 2 :rows 3
                       :x-adjust '(:left :right)
                       :y-adjust '(:center :center))
    (progress-layout switchable-layout '(nil progress-bar))
    
    (main-layout column-layout '(input-output-layout
                                 extensions-layout
                                 collect-button
                                 proposal-table 
                                 copy-button
                                 progress-layout) :adjust :center))
    
  (:default-initargs :title "Media Import"
   :visible-min-width 800
   :layout 'main-layout
   :initial-focus 'input-directory-field))

(defmethod initialize-instance :after ((self main-window) &key &allow-other-keys)
  (setf (button-enabled (slot-value self 'copy-button)) nil))


(defclass file-candidate-item (file-candidate)
  ((color :accessor file-candidate-color :initarg :color :initform :black)
   (status :accessor file-candidate-status :initform nil)))

(defmethod update-candidate-status ((self file-candidate-item))
  (with-slots (color comment status) self
    (cond ((eql status 'exists)
           (setf color :red
                 comment "File already exist"))
          ((eql status 'duplicate)
           (setf color :red
                 comment "Duplicate name"))
          ((eql status 'error)
           (setf color :red1
                 comment "Error"))
          ((eql status 'copied)
           (setf color :blue
                 comment "Copied"))
          (t
           (setf color :black
                 comment "")))))

(defun update-candidate (cand duplicates redisplay-function)
  (let ((old-status (file-candidate-status cand)))
    (cond ((fad:file-exists-p
            (file-candidate-target cand))
           (setf (file-candidate-status cand) 'exists))
          ((duplicate-p duplicates (namestring (file-candidate-target cand)))
           (setf (file-candidate-status cand) 'duplicate))
          (t
           (setf (file-candidate-status cand) nil)))
    (unless (eql old-status (file-candidate-status cand))
      (update-candidate-status cand)
      (funcall redisplay-function cand))))
  

(defmethod update-candidates ((self main-window) candidates)
  (with-slots (duplicates proposal-table) self
    (setf duplicates (make-instance 'duplicate-finder
                                      :items candidates
                                      :key (alexandria:compose #'namestring #'file-candidate-target)))
    ;; map over sequence - candidates could be a list or vector
    (map nil (lambda (cand)
               (update-candidate cand duplicates
                                 (alexandria:curry #'redisplay-collection-item proposal-table)))
          candidates)))


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
  ;; could be called from edit fields or as a button itself
  (declare (ignore data))
  (with-slots (proposal-table
               input-directory-field
               output-directory-field
               input-ext
               output-ext
               prefix
               recursive-checkbox
               exif-checkbox
               copy-button) self
      (let ((source-path (text-input-pane-text input-directory-field))
            (dest-path (text-input-pane-text output-directory-field)))
        (when (and (> (length source-path) 0) (> (length dest-path) 0))
          (cond ((not (directory-exists-p source-path))
                 (display-message "Directory ~s doesn't exist" source-path))
                ((not (directory-exists-p dest-path))
                 (display-message "Directory ~s doesn't exist" dest-path))
                (t 
                 (let* ((extensions (text-input-pane-text input-ext))
                        (new-extension (text-input-pane-text output-ext))
                        (prefix-text (text-input-pane-text prefix))
                        (r (make-instance 'renamer
                                          :source-path source-path
                                          :destination-path dest-path
                                          :prefix prefix-text
                                          :extensions extensions
                                          :new-extension new-extension
                                          :use-exif (button-selected exif-checkbox)))
                        (candidates (create-list-of-candidates r
                                                               :recursive (button-selected recursive-checkbox))))
                   (mapc (lambda (cand)
                           (change-class cand 'file-candidate-item))
                         candidates)
                   (update-candidates self candidates)
                   (setf (collection-items proposal-table)
                         candidates)
                   (setf (button-enabled copy-button) (> (length candidates) 0)))))))))


(defun file-candidate-to-row (cand)
  (list (file-candidate-source cand)
        (file-candidate-target cand)
        (file-candidate-comment cand)))

(defun color-file-candidate (lp candidate state)
  (declare (ignore lp))
  (when (eq state :normal)
    (file-candidate-color candidate)))

(defun edit-candidate-callback (item self)
  (with-slots (proposal-table) self
    (let ((message 
           (with-output-to-string (s)
             (format s "Rename ~a" (namestring (file-candidate-source item))))))
      (multiple-value-bind (fname result) 
          (prompt-for-string message :text (namestring (file-candidate-target item)))
        (when (and result
                   (not (equal fname (file-candidate-target item))))
          (setf (file-candidate-target item) (pathname fname))
          ;; update text
          (redisplay-collection-item proposal-table item)
          (update-candidates self (collection-items proposal-table)))))))


(defun on-copy-button (data self)
  (declare (ignore data))
  (with-slots (proposal-table progress-layout progress-bar) self
    ;; ask for confirmation
    (when (confirm-yes-or-no
           "Are you sure want to start copying?")
      (let* ((items (collection-items proposal-table))
             (some-dups (find-if (lambda (x) (eql (file-candidate-status x) 'duplicate)) items))
             (some-exists (find-if (lambda (x) (eql (file-candidate-status x) 'exists)) items)))
        ;; some sanity confirmations        
        (when (and (or (not some-dups)
                       (confirm-yes-or-no
                        "Where are duplicates in targets. Proceed anyway?"))
                   (or (not some-exists)
                       (confirm-yes-or-no
                        "Some existing files will be overwriten. Proceed anyway?")))
          ;; ok first make progress-bar visible
          (setf (switchable-layout-visible-child progress-layout) progress-bar)
          ;; then set the range on the progress bar equal to the number of files
          (setf (range-start progress-bar) 0
                (range-end   progress-bar) (length items)
                (range-slug-start progress-bar) 0)
          ;; start worker thread
          (mp:process-run-function "Copy files" nil #'copy-files-thread-fun self items))))))

(defmethod copy-files-thread-fun ((self main-window) items)
  "Worker function to copy files. ITEMS is an array of FILE-CANDIDATE-ITEMs."
  (flet ((copy-files-callback (i &optional error-text)
           ;; a callback provided to copy-files function from mediaimport package.
           ;; it updates the progress bar and updates the file status/color
           (apply-in-pane-process self
                                  (lambda ()
                                    (with-slots (progress-bar proposal-table)
                                        self
                                      (let ((item (aref items i)))
                                        (setf (range-slug-start progress-bar) (1+ i))
                                        (setf (file-candidate-status item)
                                              (if error-text 'error 'copied))
                                        (update-candidate-status item)
                                        (when error-text
                                          (setf (file-candidate-comment item)
                                                error-text))
                                        (redisplay-collection-item proposal-table item)))))))           
    ;; first disable all buttons
    (apply-in-pane-process self (lambda () (enable-interface self :enable nil)))
    ;; copy files with our callback
    (copy-files items :callback #'copy-files-callback)
    ;; and finally update progress, hide it and enable all buttons
    (apply-in-pane-process self
                           (lambda ()
                             (with-slots (progress-layout progress-bar) self
                               (setf (range-slug-start progress-bar) (length items))
                               (setf (switchable-layout-visible-child progress-layout) nil)
                               (enable-interface self :enable t))))))

(defmethod enable-interface ((self main-window) &key (enable t))
  "Enable or disable buttons and input fields. Called when some
background operations happened"                                                
  (with-slots (copy-button
               collect-button
               input-button
               output-button
               input-directory-field
               output-directory-field) self
    (setf (button-enabled copy-button) enable
          (button-enabled collect-button) enable
          (button-enabled input-button) enable
          (button-enabled output-button) enable
          (text-input-pane-enabled input-directory-field) enable
          (text-input-pane-enabled output-directory-field) enable)))



                         

@export
(defun main ()
  (init)
  (setf *main-window* (make-instance 'main-window))
  (display *main-window*))
