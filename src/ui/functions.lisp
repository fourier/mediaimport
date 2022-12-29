;;;; functions.lisp
;; 

(in-package #:mediaimport.ui)

(defparameter *last-used-preset-path* "Presets/lastpresetname")

(defclass file-candidate-item (file-candidate)
  ((color :accessor file-candidate-color :initarg :color :initform :black)
   (status :accessor file-candidate-status :initform nil)))

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
  (with-slots (copy-button) self
    ;; Create logger
    (create-logger self)
    ;; Disable UI elements
    (setf (button-enabled copy-button) nil)
    (toggle-custom-command self nil)
    ;; Populate presets dropdown list
    (fill-presets-list self)
    ;; Then set the default values in fields
    (fill-default-values self)
    ;; Restore history of the edit controls
    (restore-edit-controls-history self)
    ;; Recover the last preset
    (restore-from-last-preset self)
    ;; Set the UI controls state
    (toggle-use-input-file-patterns self (setting-selected self :use-file-patterns))
    (toggle-interface-on-input-patterns-change self)))
                                          


(defmethod top-level-interface-geometry-key ((self main-window))
  "Sets the key to read/write geometry position"
  (values :geometry-settings (product-name (slot-value self 'settings))))


(defmethod top-level-interface-save-geometry-p ((self main-window))
  "Returns true if need to save geometry"
  t)

(defmethod create-logger ((self main-window))
  "Creates a logger writing to the message status bar"
  (mediaimport.logger:logger-start
   #'(lambda (text)
       ;; write text from logger thread into the window thread
       (execute-with-interface-if-alive
        self
        #'(lambda ()
            (setf (titled-object-message self) text)))
       (values))))


(defmethod update-candidate-status ((self file-candidate-item))
  "Set the candidate color and comment depending on its status"
  (with-slots (color status) self
    (cond ((eql status 'exists)
           (setf color :red
                 (file-candidate-comment self) string.status-alreay-exists))
          ((eql status 'duplicate)
           (setf color :red
                 (file-candidate-comment self) string.status-duplicate))
          ((eql status 'error)
           (setf color :red1
                 (file-candidate-comment self) string.status-error))
          ((eql status 'copied)
           (setf color :blue
                 (file-candidate-comment self) string.status-copied))
          ((eql status 'processed)
           (setf color :blue
                 (file-candidate-comment self) string.status-processed))
          ((eql status 'skip)
           (setf color :grey))
          (t
           (setf color :black
                 (file-candidate-comment self) "")))))


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


(defmethod get-radio-button-panel-panes ((self main-window))
  "Returns a list of symbol names of all radio-button-panel panes of SELF"
  ;; extract slot name
  (mapcar (lambda (slot) (slot-value slot 'clos::name))
          ;; iterate over slots keep only those of type capi:text-input-choice
          (remove-if-not (lambda (slot)
                           (typep (slot-value self (slot-value slot 'clos::name)) 'capi:radio-button-panel))
                         (class-slots (class-of self)))))

(defmethod get-check-button-panel-panes ((self main-window))
  "Returns a list of symbol names of all check-button-panel panes of SELF"
  ;; extract slot name
  (mapcar (lambda (slot) (slot-value slot 'clos::name))
          ;; iterate over slots keep only those of type capi:text-input-choice
          (remove-if-not (lambda (slot)
                           (typep (slot-value self (slot-value slot 'clos::name)) 'capi:check-button-panel))
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
  "Construct the list of the column contents based on file-candidate input.
The list fills the row in the multi-column panel listing the candidates"
  (let ((target (file-candidate-target cand))
        (timestamps (file-candidate-timestamp cand)))
    (list (file-candidate-source cand)
          (if target target string.skip)
          (if (file-candidate-comment cand)
              (file-candidate-comment cand)
              "")
          (format nil "~a" (or (getf timestamps :file) ""))
          (format nil "~a" (or (getf timestamps :exif) ""))
          (format nil "~a" (or (getf timestamps :name) "")))))

(defun color-file-candidate (lp candidate state)
  (declare (ignore lp))
  (when (eq state :normal)
    (file-candidate-color candidate)))


(defmethod copy-files-thread-fun ((self main-window) items external-command delete-original open-folder)
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
    (when open-folder
      (open-folder (text-input-pane-text (slot-value self 'output-directory-edit))))))


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

(defmethod toggle-use-input-file-patterns ((self main-window) enable)
  "Toggle appropriate UI elements when input file patterns checkbox is triggered"
  (with-slots (settings-panel input-filemasks-edit) self
    (let ((exif-button-data (find-if (lambda (x) (equal (car x) :use-exif)) *settings-checkboxes*)))
      ;; disable/enable the 'use exif' checkbox
      (if enable 
          (set-button-panel-enabled-items settings-panel
                                          :set t :disable (list exif-button-data))
          (set-button-panel-enabled-items settings-panel
                                          :set t))
      ;; change the title to the input file masks field
      (setf (titled-object-title input-filemasks-edit)
            (if enable string.input-pattern string.filemasks-label)))))

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
  "Show a dialog asking for a preset name, providing SUGGESTED-NAME as a default string.
Returns either string or NIL"
  (multiple-value-bind (preset-name result)
      (prompt-for-string string.preset-name :text suggested-name)
    (when (and result (not (emptyp preset-name)))
      preset-name)))

(defmethod get-ui-values ((self main-window))
  "Returns a list of 3 lists:
- a list of conses  (symbol . string) for edit controls,
- a list of symbols with checked values for checkboxes
- a list of symbols with selected values for radioboxes"
  (let ((edits
         (mapcar (lambda (symb)
                   (cons symb (text-input-pane-text (slot-value self symb))))
                 (get-text-choice-panes self)))
        (check-buttons
         (flatten
          (mapcar (lambda (symb)
                    (let ((panel (slot-value self symb)))
                      (mapcar #'car (choice-selected-items panel))))
                  (get-check-button-panel-panes self))))
        (radio-buttons
         (flatten
          (mapcar (lambda (symb)
                    (let ((panel (slot-value self symb)))
                      (mapcar #'cdr (choice-selected-items panel))))
                  (get-radio-button-panel-panes self)))))
    (list edits check-buttons radio-buttons)))

(defmethod use-preset ((self main-window) (preset mediaimport.ui.presets:preset))
  "Fill the window from the preset provided"
  ;; collect all controls to restore from preset
  (let ((edits (mapcar (lambda (sym)
                         (cons sym (slot-value self sym)))
                       (get-text-choice-panes self)))
        (choice-panes (mapcar (curry #'slot-value self) (get-check-button-panel-panes self)))
        (radio-panes (mapcar (curry #'slot-value self) (get-radio-button-panel-panes self))))
    ;; First set the Edits
    (loop with preset-edits = (mediaimport.ui.presets:preset-edits preset)
          for (sym . ctrl) in edits
          when (gethash sym preset-edits)
          do 
          (setf (text-input-pane-text ctrl) (gethash sym preset-edits)))
    ;; Next fill the choice panes
    (loop with preset-choices = (mediaimport.ui.presets:preset-checkboxes preset)
          for choice-pane in choice-panes
          for items = (collection-items choice-pane)
          do
          (setf (choice-selection choice-pane)
                (loop for pair across items
                      for i from 0
                      when (member (car pair) preset-choices)
                      collect i)))
    ;; Finally fill the radio panes
    (loop with preset-radioboxes = (mediaimport.ui.presets:preset-radioboxes preset)
          for radio-pane in radio-panes
          for items = (collection-items radio-pane)
          do
          (setf (choice-selection radio-pane)
                (loop for pair across items
                      for i from 0
                      when (member (cdr pair) preset-radioboxes)
                      return i))))
  ;; select the loaded preset in dropdown list
  (setf (choice-selected-item
         (slot-value self 'presets-option-pane))
         (mediaimport.ui.presets:preset-name preset))
  ;; and save it as a last used
  (let ((name
         (if (string= (mediaimport.ui.presets:preset-name preset) string.default-preset-visible-name)
             nil 
             (mediaimport.ui.presets:preset-name preset))))
    (set-value (slot-value self 'settings) *last-used-preset-path* name)
    (setf (slot-value self 'current-preset-name) name)))



(defmethod fill-default-values ((self main-window))
  "Fill the window with default values"
  (todo "Not implemented")
  (with-slots (input-filemasks-edit
               pattern-edit)
      self
    ;; set default values
    (setf
     (capi-object-property input-filemasks-edit 'default-value) string.default-filemasks
     (capi-object-property pattern-edit 'default-value) string.default-output-pattern)))

(defmethod save-preset ((self main-window) &optional name)
  "Save the current window state into preset.
If no name provided save the default preset"
  (with-slots (settings) self
    ;; Update the last preset used
    (set-value settings *last-used-preset-path* name)
    (let ((edits
           (mapcar (lambda (sym)
                     (cons sym (text-input-pane-text (slot-value self sym))))
                   (get-text-choice-panes self)))
          (radioboxes
           (flatten 
            (funcall #'append
                     (mapcar (lambda (sym)
                               (mapcar
                                #'cdr
                                (choice-selected-items (slot-value self sym))))
                             (get-radio-button-panel-panes self)))))
          (checkboxes
           (flatten
            (funcall #'append
                     (mapcar (lambda (sym)
                               (mapcar
                                #'car
                                (choice-selected-items (slot-value self sym))))
                             (get-check-button-panel-panes self))))))
      (if name ;; named preset
          (mediaimport.ui.presets:create-preset
           settings name edits checkboxes radioboxes)
          (mediaimport.ui.presets:create-default-preset
           settings edits checkboxes radioboxes)))))

(defmethod delete-preset ((self main-window) name)
  (with-slots (settings) self
    (when-let ((last-name (get-value settings *last-used-preset-path*)))
      (when (string= last-name name) ;; move to default
        (set-value settings *last-used-preset-path* nil)))
    (mediaimport.ui.presets:remove-preset settings name)))
      

(defmethod restore-from-last-preset ((self main-window))
  "Restore the window state from last used preset or default"
  (with-slots (settings) self
    (when-let (preset 
               ;; Load last used preset name
               (if-let (last-preset-name
                        (get-value settings *last-used-preset-path*))
                   ;; named preset was used
                   (mediaimport.ui.presets:preset-load
                    last-preset-name
                    settings)
                 ;; load default preset
                 (mediaimport.ui.presets:load-default-preset settings)))
      (use-preset self preset))))

(defmethod fill-presets-list ((self main-window))
  "Fill the presets option pane"
  (with-slots (settings presets-option-pane) self
    (setf (collection-items presets-option-pane)
          (cons string.default-preset-visible-name
                (mediaimport.ui.presets:list-presets settings)))))

(defun make-candidates-menu (pane object x y)
  (declare (ignore object x y))
  ;; make menu only when non empty and selected
  (when-let ((items (choice-selected-items
                     (slot-value (capi:element-interface pane) ;; main-window
                                 'proposal-table))))
    (let* ((menu-items
            (list
             (make-instance 'capi:menu-item :title
                            (if (null (cdr items)) ;; only one element
                                (concatenate 'string string.open
                                             " "
                                             (ppath:basename
                                              (namestring
                                               (file-candidate-source (car items)))))
                                string.open-selected)
                            :callback 'on-candidates-menu-open
                            :callback-type :interface)
             (make-instance 'capi:menu-item :title string.copy-to-clipboard
                            :callback 'on-candidates-menu-copy
                            :callback-type :interface)
             (make-instance 'capi:menu-item :title string.rename-target-dots
                            :callback 'on-candidates-menu-rename
                            :callback-type :interface)
             (make-instance 'capi:menu-item :title string.delete-from-list
                            :callback 'on-candidates-menu-delete
                            :callback-type :interface))))
      ;; insert "Open target" menu item when only one
      ;; selected and it has a conflicting target
      (let ((item (car items)))
        (when (and (null (cdr items))
                   (eql  'exists (file-candidate-status item)))
          (setf menu-items
                (cons (car menu-items)
                      (append
                       (list
                        (make-instance
                         'capi:menu-item :title
                         (concatenate 'string string.open-target
                                      " "
                                      (ppath:basename
                                       (namestring
                                        (file-candidate-source item))))
                         :callback-type :interface
                         :callback
                         (lambda (window)
                           (declare (ignore window))
                           (view-file
                            (file-candidate-target item)))))
                       (list
                        (make-instance
                         'capi:menu-item :title string.suggest-new-target-name
                         :callback 'on-candidates-menu-optimize-target
                         :callback-type :interface
                         :enabled-function 'candidate-item-menu-has-target-p
                         :accelerator "accelerator-f3"))
                       (cdr menu-items))))))
      (make-instance 'capi:menu
                     :items
                     menu-items))))

(defmethod candidate-item-menu-enabled-p ((self main-window))
  "Called to check on main window if we have 1 candidate item.
Then the items in menu 'Item' are enabled"
  (when-let ((items (choice-selected-items
                     (slot-value self 'proposal-table))))
    (null (cdr items))))

(defmethod candidate-item-menu-has-target-p ((self main-window))
  "Called to check on main window if we have 1 candidate item with a target.
Then the items in menu 'Item' are enabled"
  (when-let ((items (choice-selected-items
                     (slot-value self 'proposal-table))))
    (let ((item (car items)))
      (and (null (cdr items))
           (file-candidate-target item)
           (not (emptyp (namestring (file-candidate-target item))))))))
  

(defun validate-input-file-pattern-string (str output-pattern)
  "Verify if the the patterns in input string STR are enough
to for the output string using OUTPUT-PATTERN"
  (when-let* ((output-patterns (mediaimport.datetime:patterns-from-string output-pattern))
              (input-patterns (mediaimport.datetime:patterns-from-string str)))
    ;; Build up the hash table from the keywords in string
    (let ((input-kws (make-hash-table)))
      (dolist (pat input-patterns)
        (setf (gethash (mediaimport.datetime:keyword-for-pattern pat) input-kws) t))
      ;; check if there is a keyword in a output list which is not yet defined in input
      (loop for pat in output-patterns
            for kw = (mediaimport.datetime:keyword-for-pattern pat)
            when (not (gethash kw input-kws)) return nil
            finally (return t)))))
    
(defmethod toggle-interface-on-input-patterns-change ((self main-window) &optional text)
  "Callback called when file masks text changed. Used to validate the pattern"
  (with-slots (input-filemasks-edit pattern-edit collect-button) self
  (unless text
    (setf text (text-input-pane-text input-filemasks-edit)))
  (let* ((output-pattern (text-input-pane-text pattern-edit))
         (validation-failed (and (setting-selected self :use-file-patterns)
                                 (not (validate-input-file-pattern-string text output-pattern)))))
    (setf (simple-pane-background input-filemasks-edit)
          (if validation-failed
              :red
              ;; clear color for normal file masks and correct patterns
              :black))
      (setf (button-enabled collect-button) (not validation-failed)))))
