;;;; ui.lisp
;; 
;; To run, execute (mediaimport.ui:main)
;;
(defpackage #:mediaimport.ui
  (:documentation "User interface definitions for MediaImport application")
  (:use #:cl #:capi #:alexandria
        #:mediaimport.utils #:mediaimport.renamer #:mediaimport.strings)
  ;; these names should be from alexandria rather than lispworks
  (:shadowing-import-from #:alexandria if-let removef when-let* appendf copy-file with-unique-names nconcf when-let)
  (:add-use-defaults t))

(in-package #:mediaimport.ui)
(annot:enable-annot-syntax)


(defconstant +proposal-table-sorting-types+
  (list
   (capi:make-sorting-description :type string.from-column
                                  :key (compose #'namestring #'file-candidate-source)
                                  :sort 'string-lessp
                                  :reverse-sort 'string-greaterp)
   (capi:make-sorting-description :type string.to-column
                                  ;; in order to do sorting we need to remember
                                  ;; what target could be nil
                                  :key (lambda (x) (namestring (or (file-candidate-target x) string.skip)))
                                  :sort 'string-lessp
                                  :reverse-sort 'string-greaterp)
   (capi:make-sorting-description :type string.comments-column
                                  :key 'file-candidate-comment
                                  :sort 'string-lessp
                                  :reverse-sort 'string-greaterp)))

;;----------------------------------------------------------------------------
;; The application interface
;;----------------------------------------------------------------------------

(define-interface cocoa-application-interface (cocoa-default-application-interface)
  ((main-window :initform nil
                :accessor main-window))
  (:menus
   (application-menu
    string.application-name
    ((:component
      ((string.about-menu
        :callback 'on-about-window
        :callback-type :none)))
#|   ;; no preferences for now
     (:component
      (("Preferences..."
        :callback 'show-preferences-window
        :callback-type :none)))
|#     
     (:component
      ()
      ;; This is a special named component where the CAPI will
      ;; attach the standard Services menu.
      :name :application-services)
     (:component
      ((string.hide-media-import
        :accelerator "accelerator-h"
        :callback-data :hidden)
       (string.hide-others
        :accelerator "accelerator-meta-h"
        :callback-data :others-hidden)
       (string.show-all
        :callback-data :all-normal))
      :callback #'(setf top-level-interface-display-state)
      :callback-type :data-interface)
     (:component
      ((string.quit
        :accelerator "accelerator-q"
        :callback 'destroy
        :callback-type :interface)))))
   (edit-menu
    string.edit
    ((:component
      ((string.undo
        :enabled-function 'active-pane-undo-p
        :callback 'active-pane-undo
        :callback-type :none)))
     (:component     
      ((string.cut
        :enabled-function 'active-pane-cut-p
        :callback 'active-pane-cut
        :callback-type :none)
       (string.copy
        :enabled-function 'active-pane-copy-p
        :callback 'active-pane-copy
        :callback-type :none)
       (string.paste
        :enabled-function 'active-pane-paste-p
        :callback 'active-pane-paste
        :callback-type :none)
       (string.select-all
        :enabled-function 'active-pane-select-all-p
        :callback 'active-pane-select-all
        :callback-type :none))))))
  (:menu-bar application-menu edit-menu)
  (:default-initargs
   :title string.application-name
   :application-menu 'application-menu
;;   :message-callback 'on-message
   :destroy-callback 'on-destroy))


(defmethod on-destroy ((self cocoa-application-interface))
  (with-slots (main-window) self
    (when main-window
      ;; Set application-interface to nil to prevent recursion back from
      ;; main-window's destroy-callback.
      (setf (slot-value main-window 'application-interface) nil)
      ;; Destroy the single  window.  When run as a delivered
      ;; application, this will cause the application to exit because it
      ;; has no more windows.
      (destroy main-window))))

;;----------------------------------------------------------------------------
;; The main window interface
;;----------------------------------------------------------------------------
(define-interface main-window ()
  ;; slots
  ((application-interface :initarg :application-interface)
   (duplicates :initform nil))
  (:menus
   ;; pop-up menu in the list of candidates
   (candidates-menu
    string.candidates
    ((string.open
      :callback 'on-candidates-menu-open
      :callback-type :interface)
     (string.copy-to-clipboard
      :callback 'on-candidates-menu-copy
      :callback-type :interface)
     (string.delete-from-list
      :callback 'on-candidates-menu-delete
      :callback-type :interface))))

  ;; ui elements
  (:panes
   (input-directory-edit text-input-pane :callback #'on-collect-button)
   (input-button push-button :text string.choose-input :callback #'on-browse-button :data 'input )
   (output-directory-edit text-input-pane :callback #'on-collect-button)
   (output-button push-button :text string.choose-output :callback #'on-browse-button :data 'output)
   (recursive-checkbox check-button :text string.search-in-subdirs)
   (exif-checkbox check-button :text string.use-exif)
   (input-filemasks-edit text-input-pane :title string.filemasks-label
                         :text string.default-filemasks
                         :visible-min-width '(:character 32)
                         :callback #'on-collect-button)
   (pattern-edit text-input-pane :title string.output-pattern
            :visible-min-width '(:character 32)
            :text string.default-output-pattern
            :callback #'on-collect-button)
   (command-checkbox check-button :text string.use-custom-command
                     :callback #'on-command-checkbox
                     :retract-callback #'on-command-checkbox)
   (command-edit text-input-pane :visible-min-width '(:character 40)
                 :callback #'on-collect-button)
   (save-script-button push-button :text string.save-script :callback #'on-save-script-button)
   (collect-button push-button :text string.collect-data :callback #'on-collect-button)
   (proposal-table multi-column-list-panel
                   :visible-min-width '(:character 100)
                   :visible-min-height '(:character 10)
                   :callback-type :item-interface ;; arguments to callback: item and interface
                   :header-args (list :selection-callback :sort) ;; "magic" callback tells it to use the sort descriptions
                   :sort-descriptions +proposal-table-sorting-types+
                   :column-function 'file-candidate-to-row
                   :color-function 'color-file-candidate
                   :action-callback 'edit-candidate-callback
                   :pane-menu candidates-menu
                   :interaction :multiple-selection
                   :columns `((:title ,string.from-column 
                               :adjust :left 
                               :visible-min-width (:character 45))
                              (:title ,string.to-column
                               :adjust :left 
                               :visible-min-width (:character 45))
                              (:title ,string.comments-column
                               :adjust :left 
                               :visible-min-width (:character 45))))
   (output-edit collector-pane :buffer-name "Output buffer")
   (copy-button push-button :text string.copy-button :callback #'on-copy-button)
   (progress-bar progress-bar))
  ;; Layout
  (:layouts
   (input-output-layout grid-layout '(input-button input-directory-edit
                                                   output-button output-directory-edit)
                        :columns 2 :rows 2
                        :x-adjust '(:right :left)
                        :y-adjust '(:center :center))
   (options-layout grid-layout '(input-filemasks-edit recursive-checkbox 
                                                      pattern-edit exif-checkbox)
                      :columns 2 :rows 2
                      :x-adjust '(:left :right)
                      :y-adjust '(:center :center))
   (command-layout row-layout '(command-checkbox command-edit save-script-button)
                   :adjust :center
                   :x-ratios '(nil 1 nil))
   (proposal-and-output-layout tab-layout '(proposal-table output-edit)
                               :print-function 'car
                               :visible-child-function 'second
                               :items `((,string.files-pane proposal-table)
                                       (,string.output-pane output-edit)))
   (progress-layout switchable-layout '(nil progress-bar))
    
   (main-layout column-layout '(input-output-layout
                                options-layout
                                command-layout
                                collect-button
                                proposal-and-output-layout
                                ;proposal-table 
                                copy-button
                                progress-layout)
                :adjust :center
                :y-ratios '(nil nil nil nil 1 nil nil)))
  ;; all other properties
  (:default-initargs
   :title string.application-name
   :visible-min-width 800
   :layout 'main-layout
   :initial-focus 'input-directory-edit
   :help-callback #'on-main-window-tooltip
   :destroy-callback #'on-destroy))


(defmethod initialize-instance :after ((self main-window) &key &allow-other-keys)
  (setf (button-enabled (slot-value self 'copy-button)) nil)
  (toggle-custom-command self nil))


(defclass file-candidate-item (file-candidate)
  ((color :accessor file-candidate-color :initarg :color :initform :black)
   (status :accessor file-candidate-status :initform nil)))


(defmethod update-candidate-status ((self file-candidate-item))
  (with-slots (color comment status) self
    (cond ((eql status 'exists)
           (setf color :red
                 comment string.status-alreay-exists))
          ((eql status 'duplicate)
           (setf color :red
                 comment string.status-duplicate))
          ((eql status 'error)
           (setf color :red1
                 comment string.status-error))
          ((eql status 'copied)
           (setf color :blue
                 comment string.status-copied))
          ((eql status 'processed)
           (setf color :blue
                 comment string.status-processed))
          ((eql status 'skip)
           (setf color :grey))
          (t
           (setf color :black
                 comment "")))))


(defun update-candidate (cand duplicates redisplay-function)
  (let ((old-status (file-candidate-status cand))
        (target (file-candidate-target cand)))
    (if target
        ;; only make sense for non-nil targets
      (cond ((fad:file-exists-p target)
             (setf (file-candidate-status cand) 'exists))
            ((duplicate-p duplicates (namestring target))
             (setf (file-candidate-status cand) 'duplicate))
            (t
             (setf (file-candidate-status cand) nil)))
        (setf (file-candidate-status cand) 'skip))
      (unless (eql old-status (file-candidate-status cand))
        (update-candidate-status cand)
        (funcall redisplay-function cand))))
  

(defmethod update-candidates ((self main-window) candidates)
  (with-slots (duplicates proposal-table) self
    (setf duplicates (make-instance 'duplicate-finder
                                    ;; only check duplicates for not-nil targets
                                      :items (remove-if (alexandria:compose #'null #'file-candidate-target)
                                                         candidates)
                                      :key (alexandria:compose #'namestring #'file-candidate-target)))
    ;; map over sequence - candidates could be a list or vector
    (map nil (lambda (cand)
               (update-candidate cand duplicates
                                 (alexandria:curry #'redisplay-collection-item proposal-table)))
          candidates)))


(defun on-about-window ()
  (capi:display-message-on-screen (capi:convert-to-screen nil)
                                  string.about-text))


(defun on-browse-button (data self)
  (with-slots (input-directory-edit output-directory-edit) self
    (let ((field nil)
          (message nil))
      (cond ((eql data 'input)
             (setf field input-directory-edit
                   message string.browse-input-dir-title))
            ((eql data 'output)
             (setf field output-directory-edit
                   message string.browse-export-dir-title)))
      (multiple-value-bind (dir result) (prompt-for-directory message)
        (when result
          (setf (capi:text-input-pane-text field) (namestring dir)))))))


(defun on-collect-button (data self)
  ;; could be called from edit fields or as a button itself
  (declare (ignore data))
  (with-slots (input-directory-edit
               output-directory-edit
               input-filemasks-edit
               pattern-edit
               recursive-checkbox
               exif-checkbox) self
    (let ((source-path (text-input-pane-text input-directory-edit))
          (dest-path (text-input-pane-text output-directory-edit)))
      (when (and (> (length source-path) 0) (> (length dest-path) 0))
        (cond ((not (directory-exists-p source-path))
               (display-message string.dir-not-exists-fmt source-path))
              ((not (directory-exists-p dest-path))
               (display-message string.dir-not-exists-fmt dest-path))
              ;; do processing only when directories are not the same
              ((not (equalp (truename source-path) (truename dest-path)))
               (let* ((masks (text-input-pane-text input-filemasks-edit))
                      (pattern-text (text-input-pane-text pattern-edit))
                      (r (make-instance 'renamer
                                        :source-path source-path
                                        :destination-path dest-path
                                        :pattern pattern-text
                                        :filemasks masks
                                        :use-exif (button-selected exif-checkbox)
                                        :recursive (button-selected recursive-checkbox))))
                 (toggle-progress self t :end 1)
                 ;; start worker thread
                 (mp:process-run-function "Collect files" nil #'collect-files-thread-fun self r))))))))
                 
                        
(defmethod collect-files-thread-fun ((self main-window) renamer)
  (with-slots (progress-bar proposal-table copy-button) self
    (let ((size 1))
      (flet ((get-total-progress (limit)
               (setq size limit)
               (apply-in-pane-process self
                                      (lambda ()
                                        (setf (range-end   progress-bar) limit))))
             (update-collect-progress (progress)
               (apply-in-pane-process self
                                      (lambda ()
                                        (setf (range-slug-start progress-bar) progress)))))

        (let ((candidates (create-list-of-candidates
                           renamer
                           :total-fun #'get-total-progress
                           :progress-fun #'update-collect-progress)))
          (mapc (lambda (cand)
                  (change-class cand 'file-candidate-item))
                candidates)
          (apply-in-pane-process self
                                 (lambda ()
                                   (update-candidates self candidates)
                                   (setf (collection-items proposal-table)
                                         candidates
                                         (button-enabled copy-button) (> (length candidates) 0)))))
        (toggle-progress self nil :end size)))))



(defun file-candidate-to-row (cand)
  (let ((target (file-candidate-target cand)))
    (list (file-candidate-source cand)
          (if target target string.skip)
          (file-candidate-comment cand))))


(defun color-file-candidate (lp candidate state)
  (declare (ignore lp))
  (when (eq state :normal)
    (file-candidate-color candidate)))


(defun edit-candidate-callback (item self)
  (with-slots (proposal-table) self
    ;; make sense only for those with target
    (when (file-candidate-target item)
      (let ((message 
             (format nil string.rename-dlg-fmt (namestring (file-candidate-source item)))))
        (multiple-value-bind (fname result) 
            (prompt-for-string message :text (namestring (file-candidate-target item)))
          (when (and result
                     (not (equal fname (file-candidate-target item))))
            (setf (file-candidate-target item) (pathname fname))
            ;; update text
            (redisplay-collection-item proposal-table item)
            (update-candidates self (collection-items proposal-table))))))))


(defun on-copy-button (data self)
  (declare (ignore data))
  (with-slots (proposal-table command-checkbox) self
    (let ((do-copy (button-selected command-checkbox)))
      ;; ask for confirmation
      (when (confirm-yes-or-no
             string.start-copy-confirmation)
        (let* ((items (collection-items proposal-table))
               (some-dups (find-if (lambda (x) (eql (file-candidate-status x) 'duplicate)) items))
               (some-exists (find-if (lambda (x) (eql (file-candidate-status x) 'exists)) items)))
          ;; some sanity confirmations        
          (when (and (or (not some-dups)
                         (confirm-yes-or-no
                          string.duplicates-exist-confirmation))
                     (or (not some-exists)
                         (confirm-yes-or-no
                          string.overwrite-confirmation)))
            (toggle-progress self t :end (length items))
            ;; start worker thread
            (mp:process-run-function "Copy files"
                                     nil
                                     #'copy-files-thread-fun
                                     self
                                     items do-copy)))))))


(defmethod copy-files-thread-fun ((self main-window) items external-command)
  "Worker function to copy/apply command to files.
ITEMS is an array of FILE-CANDIDATE-ITEMs. EXTERNAL-COMMAND is a boolean flag;
if T execute command from command-edit, otherwise just copy files"
  (flet ((copy-files-callback (i &optional error-text)
           ;; a callback provided to copy-files function from mediaimport package.
           ;; it updates the progress bar and updates the file status/color
           (apply-in-pane-process self
                                  (lambda ()
                                    (with-slots (progress-bar proposal-table)
                                        self
                                      (let ((item (aref items i)))
                                        (setf (range-slug-start progress-bar) (1+ i))
                                        (unless (eql (file-candidate-status item) 'skip)
                                          (setf (file-candidate-status item)
                                                (cond (error-text 'error)
                                                      (external-command 'processed)
                                                      (t 'copied))))
                                        (update-candidate-status item)
                                        (when error-text
                                          (setf (file-candidate-comment item)
                                                error-text))
                                        (redisplay-collection-item proposal-table item)))))))           
    ;; copy files with our callback
    (if external-command
        ;; command text
        (let ((cmd (text-input-pane-text (slot-value self 'command-edit))))
          ;; validate
          (multiple-value-bind (result text)
              (validate-command-string cmd)
            (if (not result)
                ;; error message
                (display-message text)
                ;; otherwise process
                (apply-command-to-files items
                                      cmd
                                      :callback #'copy-files-callback
                                      :stream (collector-pane-stream (slot-value self 'output-edit))))))
        (copy-files items :callback #'copy-files-callback))
    ;; and finally update progress, hide it and enable all buttons
    (toggle-progress self nil :end (length items))))


(defmethod toggle-progress ((self main-window) enable &key (start 0) end)
  (apply-in-pane-process self
                         (lambda ()
                           (with-slots (progress-bar progress-layout) self
                             (if enable
                                 ;; ok first make progress-bar visible
                                 (setf (switchable-layout-visible-child progress-layout) progress-bar
                                       ;; then set the range on the progress bar equal to the number of files
                                       (range-start progress-bar) start
                                       (range-end   progress-bar) end
                                       (range-slug-start progress-bar) 0)
                                 ;; disable
                                 (setf (range-slug-start progress-bar) end
                                       (switchable-layout-visible-child progress-layout) nil))
                             ;; enable/disable buttons
                             (enable-interface self :enable (not enable))))))
  

(defmethod enable-interface ((self main-window) &key (enable t))
  "Enable or disable buttons and input fields. Called when some
background operations happened"                                                
  (with-slots (copy-button
               collect-button
               input-button
               output-button
               input-directory-edit
               output-directory-edit) self
    (setf (button-enabled copy-button) enable
          (button-enabled collect-button) enable
          (button-enabled input-button) enable
          (button-enabled output-button) enable
          (text-input-pane-enabled input-directory-edit) enable
          (text-input-pane-enabled output-directory-edit) enable)))


(defmethod on-main-window-tooltip ((self main-window) pane type key)
  (when (eq type :tooltip) ;; the only possible type on Cocoa
    (ecase key
      (pattern-edit string.patten-tooltip)
      (command-edit string.command-tooltip))))


(defmethod on-save-script-button (data (self main-window))
  "Callback called on Save script button"
  (declare (ignore data))
  (with-slots (proposal-table) self
    (let ((items (collection-items proposal-table))
          (filename (prompt-for-file string.prompt-save-script :operation :save :filter "*.sh")))
      (when filename
        (with-open-file (stream filename :direction :output :if-exists :supersede)
          (apply-command-to-files items
                                  (text-input-pane-text
                                   (slot-value self 'command-edit))
                                  :stream stream
                                  :script t))))))

  
(defmethod on-destroy ((self main-window))
  "Callback called when closing the main window"
  (with-slots (application-interface) self
    (when application-interface
      ;; Set main-window to nil to prevent recursion back from
      ;; application-interface's destroy-callback.
      (setf (main-window application-interface) nil)
      ;; Quit by destroying the application interface.
      (capi:destroy application-interface))))


(defmethod on-command-checkbox (data (self main-window))
  "Callback called when toggled command checkbox"
  (declare (ignore data))
  (with-slots (command-checkbox) self
    (toggle-custom-command self (button-selected command-checkbox))))


(defmethod toggle-custom-command ((self main-window) enable)
  "Toggle appropriate UI elements when command checkbox is triggered"
  (with-slots (save-script-button command-edit copy-button) self
    (setf (button-enabled save-script-button) enable
          (text-input-pane-enabled command-edit) enable
          (item-text copy-button) (if enable string.process-button string.copy-button))))


(defmethod on-candidates-menu-copy ((self main-window))
  (with-slots (proposal-table) self
    (when-let ((selected (choice-selected-items proposal-table)))
      (set-clipboard self
                     (format nil "~{~A~^~%~}"
                             (mapcar #'file-candidate-source selected))))))


(defmethod on-candidates-menu-delete ((self main-window))
  (with-slots (proposal-table) self
    (when-let ((selected (choice-selected-items proposal-table)))
      (when (confirm-yes-or-no string.remove-files-fmt
                               (mapcar #'file-candidate-source selected))
        (remove-items proposal-table selected)))))


(defmethod on-candidates-menu-open ((self main-window))
  (with-slots (proposal-table) self
    (when-let ((selected (choice-selected-items proposal-table)))
      ;; [[NSWorkspace sharedWorkspace] openFile:path];
      (display-message string.open))))


@export
(defun main ()
  (init)
  (let ((application (make-instance 'cocoa-application-interface)))
    (set-application-interface application)
    (let ((main-window (make-instance 'main-window
                                      :application-interface application)))
      (setf (main-window application) main-window)
      (display main-window))))
