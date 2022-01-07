;;;; callbacks.lisp
;; 

(in-package #:mediaimport.ui)


(defun on-about-window ()
  (capi:display-message-on-screen
   (capi:convert-to-screen nil)
   string.about-text (mediaimport.version:version-string)))


(defun on-collect-button (data self)
  ;; could be called from edit fields or as a button itself
  (declare (ignore data))
  (with-slots (input-directory-edit
               output-directory-edit
               input-filemasks-edit
               comparison-options-panel
               pattern-edit) self
    (let ((source-path (text-input-pane-text input-directory-edit))
          (dest-path (text-input-pane-text output-directory-edit)))
      ;; verify what paths are not empty
      (when (and (> (length source-path) 0) (> (length dest-path) 0))
        ;; some sanity checks. if directories exists at all
        (cond ((not (directory-exists-p source-path))
               (display-message string.dir-not-exists-fmt source-path))
              ((not (directory-exists-p dest-path))
               (display-message string.dir-not-exists-fmt dest-path))
              ;; no import to the same directory
              ((equalp (truename source-path) (truename dest-path))
               (display-message string.source-dest-must-differ))
              ;; do processing only when directories are not the same
              ((not (equalp (truename source-path) (truename dest-path)))
               (let* ((masks (text-input-pane-text input-filemasks-edit))
                      (pattern-text (text-input-pane-text pattern-edit))
                      (comparison-type (cdr (choice-selected-item comparison-options-panel)))
                      (r (make-instance 'renamer
                                        :source-path source-path
                                        :destination-path dest-path
                                        :pattern pattern-text
                                        :filemasks masks
                                        :use-exif (setting-selected self :use-exif)
                                        :comparison-type comparison-type
                                        :recursive (setting-selected self :search-in-subdirs))))
                 ;; save the edit fields to the history
                 (save-edit-controls-history self)
                 ;; toggle progress bar indication
                 (toggle-progress self t :end 1)
                 ;; start worker thread
                 (mp:process-run-function "Collect files" nil #'collect-files-thread-fun self r))))))))


(defun on-candidate-dblclick (item self)
  ;; make sense only for those with target
  (when (file-candidate-target item)
    (mediaimport.utils:view-file (namestring (file-candidate-source item)))))

(defun on-copy-button (data self)
  (declare (ignore data))
  (with-slots (proposal-table) self
    (let ((do-copy (setting-selected self :use-custom-command))
          (delete-original (setting-selected self :move-instead-of-copy))
          (open-folder (setting-selected self :open-folder-after)))
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
                          string.overwrite-confirmation))
                     (or (not delete-original)
                         (confirm-yes-or-no
                          string.delete-original-confirmation)))
            (toggle-progress self t :end (length items))
            ;; start worker thread
            (mp:process-run-function "Copy files"
                                     nil
                                     #'copy-files-thread-fun
                                     self
                                     items do-copy delete-original open-folder)))))))

(defmethod on-main-window-tooltip ((self main-window) pane type key)
  (when (eq type :tooltip) ;; the only possible type on Cocoa
    (case key
      (pattern-edit string.pattern-tooltip)
      (command-edit string.command-tooltip)
      (input-filemasks-edit string.filemasks-tooltip))))


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


(defmethod on-settings-checkbox-selected (data (self main-window))
  "Callback called when selected one of settings checkboxes"
  (case (car data)
    (:use-custom-command
     (toggle-custom-command self t))
    (t nil)))


(defmethod on-settings-checkbox-retracted (data (self main-window))
  "Callback called when retracted selection of settings checkboxes"
  (case (car data)
    (:use-custom-command
     (toggle-custom-command self nil))
    (t nil)))


(defmethod on-candidates-menu-copy ((self main-window))
  (with-slots (proposal-table) self
    (when-let ((selected (choice-selected-items proposal-table)))
      (set-clipboard self
                     (format nil "~{~A~^~%~}"
                             (mapcar #'file-candidate-source selected))))))


(defmethod on-candidates-menu-delete ((self main-window))
  (with-slots (proposal-table) self
    (when-let ((selected (choice-selected-items proposal-table)))
      (when (confirm-yes-or-no string.remove-files-question
                               (mapcar #'file-candidate-source selected))
        (remove-items proposal-table selected)))))


(defmethod on-candidates-menu-open ((self main-window))
  "Contex menu item handler, open all selected files with as in finder"
  (with-slots (proposal-table) self
    (when-let ((selected (choice-selected-items proposal-table)))
      (mapc (compose #'mediaimport.utils:view-file #'namestring #'file-candidate-source) selected))))

(defmethod on-candidates-menu-rename ((self main-window))
  (with-slots (proposal-table) self
    ;; make sense only for those with target
    (when-let ((items (choice-selected-items proposal-table)))
      (let ((item (car items)))
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
                (update-candidates self (collection-items proposal-table))))))))))

(defun on-command-edit-changed (str edit interface caret-pos)
  "Callback called when command text changed. Used to validate the command"
  (declare (ignore interface caret-pos))
  (setf (simple-pane-foreground edit)
        (if (validate-command-string str) :black :red)))

(defmethod on-clear-history-button ((self cocoa-application-interface))
  "Clear History menu item handler"
  (clear-history (main-window self)))

(defmethod on-clear-history-button ((self main-window))
  "Clear History menu item handler"
  (clear-history self))

(defmethod on-new-preset-button (data (self main-window))
  "Save preset button handler"
  (when-let (name (preset-name-dialog string.default-preset-name))
    (display-message name)))

(defmethod on-modify-presets-button (data (self main-window))
  "Manage presets button handler"
  (display (make-instance 'presets-window)))
