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
  (process-button . "Process...")
  (files-pane . "Files")
  (output-pane . "Output")
  (status-alreay-exists . "File already exist")
  (status-duplicate . "Duplicate name")
  (status-error . "Error")
  (status-copied . "Copied")
  (status-processed . "Processed")
  (about-text . "Media Import utility~%Copyright (c) 2016 Alexey Veretennikov")
  (browse-input-dir-title . "Import from")
  (browse-export-dir-title . "Export to")
  (dir-not-exists-fmt . "Directory ~s doesn't exist")
  (rename-dlg-fmt . "Rename ~a")
  (start-copy-confirmation . "Are you sure want to start copying?")
  (duplicates-exist-confirmation . "Where are duplicates in targets. Proceed anyway?")
  (overwrite-confirmation . "Some existing files will be overwriten. Proceed anyway?")
  (patten-tooltip . "The output file pattern.
Example: \"{YYYY}-{MM}-{DD}/Photo-{hh}_{mm}.jpg\".
If extension provided, use this extension, otherwise if no extension provided or it is a wildcard .* use original extensions.
Possible templates:
{YYYY}  - year, like 1999
{MM}    - month, like 01
{DD}    - day, like 31
{MONTH} - month name, like January
{MON}   - 3 letters month abbreviation, like Nov
{МЕСЯЦ} - russian month name
{МЕС}   - russian month abbreviation
{hh}    - hour, in 24-hours format
{mm}    - minute
{ss}    - second")
  (command-tooltip . "The custom command pattern.
Example: \"convert -resize 1024x768 {SOURCE} {TARGET}
Possible templates:
{SOURCE} full path to the source file
{TARGET} full path to the target file")
  (prompt-save-script . "Choose script name to save")
  (remove-files-fmt . "Remove these files from the list?~%~{~A~^~%~}")
  )