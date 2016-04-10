;;;; strings.lisp
;; 
;;
(defpackage #:mediaimport.strings
  (:documentation "String constants")
  (:use #:cl #:mediaimport.utils)
  (:nicknames #:strings))
  
  
(in-package #:mediaimport.strings)

(defmacro export-strings (&body string-list)
  `(define-resource string ,@string-list))

;(define-resource string
;  (to . "To: ") 
;  (from . "From: ")
;  (message . "hello"))


(export-strings
  ;; from renamer package
  (same-as- . "Same as: ")
  (source-not-provided . "{SOURCE} template argument is not provided")
  (failed-fmt . "Failed: ~a")
  ;; from ui package
  (application-name . "Media Import")
  (from-column . "From")
  (to-column . "To")
  (comments-column . "Comments")
  (skip . "(skip)")
  (about-menu . "About Media Import")
  (hide-media-import . "Hide Media Import")
  (hide-others . "Hide Others")
  (show-all . "Show All")
  (quit . "Quit Media Import")
  (edit . "Edit")
  (undo . "Undo")
  (cut . "Cut")
  (copy . "Copy")
  (paste . "Paste")
  (select-all . "Select All")
  (candidates . "Candidates")
  (open . "Open")
  (copy-to-clipboard . "Copy to clipboard")
  (delete-from-list . "Delete from the list")
  (choose-input . "Choose Input directory...")
  (choose-output . "Choose Output directory...")
  (search-in-subdirs . "Search in subdirectories")
  (use-exif . "Use EXIF for JPG")
  (filemasks-label . "Comma-separated list of file masks, like \"*.jpg,*.png\"")
  (output-pattern . "Output pattern")
  (default-output-pattern . "{YYYY}-{MM}-{DD}/Photo-{hh}_{mm}.jpg")
  (use-custom-command . "Use custom command")
  (save-script . "Save as script")
  (collect-data . "Collect data")
  (copy-button . "Copy...")
  (files-pane . "Files")
  (output-pane . "Output")
  )