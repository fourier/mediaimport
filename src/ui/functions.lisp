;;;; ui-code.lisp
;; 

(in-package #:mediaimport.ui)

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

(defmethod initialize-instance :after ((self main-window) &key &allow-other-keys)
  "Constructor for the main-window class"
  (with-slots (copy-button
               input-filemasks-edit
               pattern-edit)
      self
    (setf (button-enabled copy-button) nil)
    (toggle-custom-command self nil)
    ;; set default values
    (setf
     (capi-object-property input-filemasks-edit 'default-value) string.default-filemasks
     (capi-object-property pattern-edit 'default-value) string.default-output-pattern)

    (restore-edit-controls-history self)))
    
(defmethod top-level-interface-geometry-key ((self main-window))
  "Sets the key to read/write geometry position"
  (values :geometry-settings (product (slot-value self 'settings))))


(defmethod top-level-interface-save-geometry-p ((self main-window))
  "Returns true if need to save geometry"
  t)


(defclass file-candidate-item (file-candidate)
  ((color :accessor file-candidate-color :initarg :color :initform :black)
   (status :accessor file-candidate-status :initform nil)
   (comment :accessor file-candidate-comment :initform "" :initarg :comment)))


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

(defmethod get-text-choice-panes ((self main-window))
  "Returns a list of symbol names of all text-input-choice panes of SELF"
  ;; extract slot name
  (mapcar (lambda (slot) (slot-value slot 'clos::name))
          ;; iterate over slots keep only those of type capi:text-input-choice
          (remove-if-not (lambda (slot)
                           (typep (slot-value self (slot-value slot 'clos::name)) 'capi:text-input-choice))
                         (class-slots (class-of self)))))


(defmethod get-choice-panes ((self main-window))
  "Returns a list of symbol names of all text-input-choice panes of SELF"
  ;; extract slot name
  (mapcar (lambda (slot) (slot-value slot 'clos::name))
          ;; iterate over slots keep only those of type capi:text-input-choice
          (remove-if-not (lambda (slot)
                           (typep (slot-value self (slot-value slot 'clos::name)) 'capi:text-input-choice))
                         (class-slots (class-of self)))))

;;(comparison-type (cdr (choice-selected-item comparison-options-panel)))

(defmethod restore-edit-controls-history ((self main-window))
  "Replaces the contents of all edit-choice fields with the stored"
  (with-slots (settings) self
    ;; for each edit
    (mapc (lambda (edit)
            ;; get the default value
            (let* ((default-value 
                    (capi-object-property (slot-value self edit) 'default-value))
                   ;; get the history
                   (history
                    ;; if default value is not empty
                    (if (> (length default-value) 0)
                        (get-value settings (symbol-name edit) (list default-value))
                        ;; otherwise just take the history
                        (get-value settings (symbol-name edit)))))
              ;; no need to set the empty history
              (unless (null history)
                (setf (collection-items (slot-value self edit)) history
                      (text-input-pane-text (slot-value self edit)) (car history)))))
          (get-text-choice-panes self))))


(defmethod save-edit-controls-history ((self main-window))
  "Saves the history of all edit fields"
  (with-slots (settings) self
    ;; for each edit
    (mapc (lambda (edit)
            ;; get the default value
            (let* ((txt (text-input-pane-text (slot-value self edit)))
                   (items (map 'list #'identity (collection-items (slot-value self edit)))))
              (when (> (length txt) 0)
                ;; move current value to the top in history
                (setf items (push-top txt items :test #'string-equal))
                ;; store items
                (set-value settings (symbol-name edit) items)
                ;; and finally update the ui
                (setf (collection-items (slot-value self edit)) items))))
          (get-text-choice-panes self))))

  
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


(defmethod copy-files-thread-fun ((self main-window) items external-command delete-original &optional (open-folder nil))
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
                                      :stream (collector-pane-stream (slot-value self 'output-edit))
                                      :delete-original delete-original))))
        (copy-files items :callback #'copy-files-callback :delete-original delete-original))
    ;; update progress, hide it and enable all buttons
    (toggle-progress self nil :end (length items))
    ;; and finally open folder if the setting requested
;;    (when open-folder (open-folder
))


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
               input-directory-edit
               output-directory-edit
               input-filemasks-edit
               pattern-edit
               settings-panel) self
    (setf (button-enabled copy-button) enable
          (button-enabled collect-button) enable
          (text-input-pane-enabled input-directory-edit) enable
          (text-input-pane-enabled output-directory-edit) enable
          (text-input-pane-enabled input-filemasks-edit) enable
          (text-input-pane-enabled pattern-edit) enable
          (simple-pane-enabled settings-panel) enable)))


(defmethod setting-selected ((self main-window) option)
  "Check if the settings checkbox selected. OPTION is one of
symbols in *settings-checkboxes*"
  (with-slots (settings-panel) self
    (when-let (selected (mapcar #'car (choice-selected-items settings-panel)))
      (member option selected))))


(defmethod toggle-custom-command ((self main-window) enable)
  "Toggle appropriate UI elements when command checkbox is triggered"
  (with-slots (save-script-button command-edit copy-button) self
    (setf (button-enabled save-script-button) enable
          (text-input-pane-enabled command-edit) enable
          (item-text copy-button) (if enable string.process-button string.copy-button))))

(defmethod clear-history ((self main-window))
  "Launch the Clear history dialog and clear history for selected edits."
  (with-slots (settings) self
    (when-let (clear-from
               ;; list of edits - output from the dialog
               (prompt-with-list (get-text-choice-panes self)
                                 string.clear-history-dialog-title
                                 :interaction :multiple-selection
                                 :choice-class 'button-panel
                                 :print-function
                                 (compose #'titled-object-title (curry #'slot-value self))
                                 :pane-args
                                 '(:layout-class column-layout)))
      ;; for every edit selected set nil corresponding setting
      (mapc (lambda (edit)
              (set-value settings (symbol-name edit) nil))
            clear-from)
      (restore-edit-controls-history self))))


(defun preset-name-dialog (suggested-name)
  (multiple-value-bind (preset-name result)
      (prompt-for-string string.preset-name :text suggested-name)
    (when (and result (not (emptyp preset-name)))
      preset-name)))

(defmethod initialize-instance :after ((self presets-window) &key &allow-other-keys)
  "Constructor for the presets-window class"
  )
