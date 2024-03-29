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
  (open-target . "Open Target")
  (open-selected . "Open Selected")
  (copy-to-clipboard . "Copy to clipboard")
  (delete-from-list . "Delete from the list")
  (choose-input . "From Directory")
  (choose-output . "To Directory")
  (search-in-subdirs . "Search in subdirectories")
  (use-exif . "Use EXIF for JPG")
  (filemasks-label . "File masks")
  (default-filemasks . "*.jpg")
  (output-pattern . "Output pattern")
  (default-output-pattern . "{YYYY}-{MM}-{DD}/Photo-{hh}_{mm}.jpg")
  (use-custom-command . "Use custom command")
  (custom-command . "Custom command")
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
  (about-text . "Media Import utility ~a~%Copyright (c) 2016-2022 Alexey Veretennikov")
  (dir-not-exists-fmt . "Directory ~s doesn't exist")
  (rename-dlg-fmt . "Rename ~a")
  (start-copy-confirmation . "Are you sure want to start copying?")
  (duplicates-exist-confirmation . "Where are duplicates in targets. Proceed anyway?")
  (overwrite-confirmation . "Some existing files will be overwriten. Proceed anyway?")
  (pattern-tooltip . "The output file pattern.
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
  (filemasks-tooltip . "Comma-separated list of file masks, like \"*.jpg,*.png\"")  
  (prompt-save-script . "Choose script name to save")
  (remove-files-question . "Remove selected files from the list?")
  (clear-history-menu . "Clear history...")
  (clear-history-dialog-title . "Clear history")
  (clear-history-question . "Clear history for selected fields?")
  (source-dest-must-differ . "Source and destination directories must be different")
  (move-instead-of-copy . "Delete original file")
  (delete-original-confirmation . "Original files will be removed. Proceed?")
  (settings . "Settings")
  (comparison-options . "Comparison options")
  (crc-comparison . "Compare CRC" )
  (binary-comparison . "Binary comparison")
  (quick-comparison . "Quick comparison")
  (new-preset-dots . "New...")
  (rename-preset-dots . "Rename...")
  (delete-preset-dots . "Delete...")
  (presets . "Presets")
  (preset-name . "Preset name")
  (default-preset-name . "New preset")
  (rename-target-dots . "Rename target...")
  (open-folder-after . "Open folder after copy")
  (current . "Current")
  (default-preset-visible-name . "<<Default>>")
  (reserved-preset-name . "This preset name is preserved for default preset")
  (warning . "Warning")
  (really-overwrite-preset . "Do you really want to overwrite existing preset with the same name?")
  (really-want-delete-preset . "Do you really want to delete preset ~s ?")
  (item . "Item")
  (suggest-new-target-name . "Suggest new target name")
  (timestamp-exif . "Timestamp (EXIF)")
  (timestamp-file . "Timestamp (File)")
  (timestamp-name . "Timestamp (Name)")
  (input-pattern . "Input pattern")
  (input-pattern-tooltip . "The input file pattern.
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
  (use-file-patterns . "Parse datetime from file names")
  )
