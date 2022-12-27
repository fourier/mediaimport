;;;; interfaces.lisp
;; 
;; To run, execute (mediaimport:main)
;;
(defpackage #:mediaimport.ui
  (:documentation "User interface definitions for MediaImport application")
  (:use #:cl #:capi #:alexandria
        #:mediaimport.utils #:mediaimport.renamer #:mediaimport.strings
        #:mediaimport.settings)
  ;; these names should be from alexandria rather than lispworks
  (:shadowing-import-from #:alexandria
   if-let removef when-let* appendf copy-file with-unique-names nconcf when-let)
  (:add-use-defaults t)
  (:export main-window #+cocoa cocoa-application-interface))

(in-package #:mediaimport.ui)


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

(defparameter *settings-checkboxes*
  `((:use-file-patterns . ,string.use-file-patterns)
    (:use-exif . ,string.use-exif)
    (:search-in-subdirs . ,string.search-in-subdirs)
    (:use-custom-command . ,string.use-custom-command)
    (:move-instead-of-copy . ,string.move-instead-of-copy)
    (:open-folder-after . ,string.open-folder-after))
  "Data for the settings checkboxes - symbol to string mapping")


(defparameter *comparison-options*
  (list (cons string.crc-comparison  :crc)
        (cons string.binary-comparison :binary)
        (cons string.quick-comparison :quick))
  "Data for the comparison options radio buttons")

;;----------------------------------------------------------------------------
;; Main Window/application base interface
;;----------------------------------------------------------------------------
(define-interface main-window-base () ()
  (:menus
   (application-menu
    string.application-name
    ((:component
      ((string.about-menu
        :callback 'on-about-window
        :callback-type :none)))
     (:component
      ((string.clear-history-menu
        :callback 'on-clear-history-button
        :callback-type :interface)))
#|   ;; no preferences for now
     (:component
      (("Preferences..."
        :callback 'show-preferences-window
        :callback-type :none)))
|#     
     #+cocoa
     (:component
      ()
      ;; This is a special named component where the CAPI will
      ;; attach the standard Services menu.
      :name :application-services)
     #+cocoa
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
   (item-menu
    string.item
    ((:component
      ((string.rename-target-dots
        :callback 'on-candidates-menu-rename
        :callback-type :interface
        :enabled-function 'candidate-item-menu-enabled-p
        :accelerator "accelerator-f2")))
     (:component
      ((string.suggest-new-target-name
        :callback 'on-candidates-menu-optimize-target
        :callback-type :interface
        :enabled-function 'candidate-item-menu-has-target-p
        :accelerator "accelerator-f3")))))))

;;----------------------------------------------------------------------------
;; The application interface
;;----------------------------------------------------------------------------

(define-interface cocoa-application-interface (#+:cocoa cocoa-default-application-interface
                                                        main-window-base)
  ((main-window :initform nil
                :accessor main-window))
  (:menu-bar application-menu)
  (:default-initargs
   :title string.application-name
   :application-menu 'application-menu
;;   :message-callback 'on-message
   :destroy-callback 'on-destroy))


;;----------------------------------------------------------------------------
;; The main window interface
;;----------------------------------------------------------------------------
(define-interface main-window (main-window-base)
  ;; slots
  ((application-interface :initarg :application-interface)
   (duplicates :initform nil)
   (settings :initform (make-app-settings)
             :reader main-window-settings)
   (current-preset-name :initform nil :reader main-window-current-preset-name
                        :documentation "Currently selected preset name. If nil it is the default preset"))

  ;; ui elements
  (:panes
   (input-directory-edit text-input-choice :callback 'on-collect-button
                         :title string.choose-input
                         :buttons 
                         '(:browse-file (:directory t :image :std-file-open) :ok nil))
   (output-directory-edit text-input-choice :callback 'on-collect-button
                          :title string.choose-output
                         :buttons 
                         '(:browse-file (:directory t :image :std-file-open) :ok nil))
   (input-filemasks-edit text-input-choice :title string.filemasks-label
                         :visible-max-width nil
                         :visible-max-height nil
                         :visible-min-height '(:character 1)
                         :callback 'on-collect-button)
   (pattern-edit text-input-choice :title string.output-pattern
                 :visible-max-width nil
                 :visible-max-height '(:character 1)
            :callback 'on-collect-button)
   (command-edit text-input-choice :visible-min-width '(:character 40)
                 :title string.custom-command
                 :callback 'on-collect-button
                 :text-change-callback 'on-command-edit-changed)
   (save-script-button push-button :text string.save-script :callback 'on-save-script-button)
   (settings-panel check-button-panel
                   :visible-max-width nil
                   :visible-max-height nil
                   :items *settings-checkboxes*
                   :print-function #'cdr
                   :selection-callback 'on-settings-checkbox-selected
                   :retract-callback 'on-settings-checkbox-retracted
                   :layout-class 'grid-layout
                   :layout-args '(:columns 3))
   (comparison-options-panel radio-button-panel
                 :title string.comparison-options
                 :title-position :frame
                 :visible-max-width nil
                 :visible-max-height nil
                 :items *comparison-options*
                 :print-function #'car
                 :layout-class 'capi:row-layout
                 :layout-args '(:uniform-size-p t :x-adjust (:left :center :right)))
   (presets-option-pane option-pane :title string.current :test-function #'string=
                        :selection-callback #'on-preset-change-callback
                        :callback-type :item-interface)
   (new-preset-button push-button :text string.new-preset-dots :callback 'on-new-preset-button)
   (rename-preset-button push-button :text string.rename-preset-dots :callback 'on-rename-preset-button)
   (delete-preset-button push-button :text string.delete-preset-dots :callback 'on-delete-preset-button)   
   (proposal-table multi-column-list-panel
                   :visible-min-width '(:character 100)
                   :visible-min-height '(:character 2)
                   :callback-type :item-interface ;; arguments to callback: item and interface
                   :header-args (list :selection-callback :sort) ;; "magic" callback tells it to use the sort descriptions
                   :sort-descriptions +proposal-table-sorting-types+
                   :column-function 'file-candidate-to-row
                   :color-function 'color-file-candidate
                   :action-callback 'on-candidate-dblclick
                   :pane-menu #'make-candidates-menu
                   :interaction :extended-selection
                   :columns `((:title ,string.from-column 
                               :adjust :left
                               :visible-min-width (:character 45))
                              (:title ,string.to-column
                               :adjust :left 
                               :visible-min-width (:character 45))
                              (:title ,string.comments-column
                               :adjust :left
                               :visible-min-width (:character 45))
                              (:title ,string.timestamp-file
                               :adjust :left
                               :visible-min-width (:character 20))
                              (:title ,string.timestamp-exif
                               :adjust :left
                               :visible-min-width (:character 20))
                              (:title ,string.timestamp-name
                               :adjust :left
                               :visible-min-width (:character 20))))
   (output-edit collector-pane :buffer-name "Output buffer")
   (collect-button push-button :text string.collect-data :callback 'on-collect-button)
   (copy-button push-button :text string.copy-button :callback 'on-copy-button)
   (progress-bar progress-bar))
  (:layouts
   (input-output-layout column-layout '(input-directory-edit output-directory-edit))
   (left-edits-layout column-layout '(input-filemasks-edit pattern-edit))
   (options-layout row-layout '(left-edits-layout settings-panel)
                   :x-adjust '(:right :left)
                   :x-ratios '(1 nil)
                   :title string.settings
                   :title-position :frame)
   (presets-layout row-layout '(presets-option-pane 
                                 new-preset-button rename-preset-button delete-preset-button)
                   :columns 2
                   :title string.presets
                   :title-position :frame)
   (option-and-presets-layout row-layout '(comparison-options-panel presets-layout) :x-ratios '(nil 1))
   (command-layout row-layout '(command-edit save-script-button)
                   :adjust :center
                   :x-ratios '(1 nil))
   (proposal-and-output-layout tab-layout '(proposal-table output-edit)
                               :print-function 'car
                               :visible-child-function 'second
                               :items (list (list string.files-pane 'proposal-table)
                                            (list string.output-pane 'output-edit)))
   (progress-layout switchable-layout '(nil progress-bar))
   (action-buttons-layout row-layout '(collect-button copy-button))
   (main-layout column-layout '(input-output-layout
                                options-layout
                                option-and-presets-layout
                                command-layout
                                proposal-and-output-layout
                                action-buttons-layout
                                progress-layout)
                :adjust :center
                :internal-border 10
                :y-ratios '(nil nil nil nil 1 nil nil)))
  ;; all other properties
  #-cocoa (:menu-bar application-menu item-menu)

  (:default-initargs
   :title string.application-name
   :visible-min-width 800
   ;; TODO: implement message area logging
   :message-area t
   :layout 'main-layout
   :initial-focus 'input-directory-edit
   :help-callback #'on-main-window-tooltip
   :destroy-callback #'on-destroy))


