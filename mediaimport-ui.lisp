;;;; mediaimport.lisp
(defpackage #:mediaimport-ui
  (:use #:cl #:capi #:mediaimport)
  (:add-use-defaults t))

(in-package #:mediaimport-ui)
(annot:enable-annot-syntax)

(define-interface main-window ()
  ()
  (:panes
   (input-label display-pane :text "Input directory")
   (input-directory-field text-input-pane)
   (input-button push-button :text "Browse")
   (output-label display-pane :text "Output directory")   
   (output-directory-field text-input-pane)
   (output-button push-button :text "Browse")
   (input-ext text-input-pane :title "Extension" :text "*.*")
   (output-ext text-input-pane :title "Output extension")
   (collect-button push-button :text "Collect data")
   (proposal-table multi-column-list-panel
      :visible-min-width 600
      :visible-min-height :text-height
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
   :visible-min-width 600
   :layout 'main-layout))


@export
(defun main ()
  (init)
  (display (make-instance 'main-window)))
