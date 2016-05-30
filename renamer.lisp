;;;; renamer.lisp

(defpackage #:mediaimport.renamer
  (:documentation "Application logic related to searching and renaming")
  (:use #:cl #:cl-annot.class #:alexandria
   #:mediaimport.utils #:mediaimport.datetime #:mediaimport.strings))

(in-package #:mediaimport.renamer)
(annot:enable-annot-syntax)


(defconstant +timestamp-format-mapping+
  (make-hash-table :test #'string=)
  "Hash table containing mapping between timestamp pseudo-formatting
and (FORMAT formatting + getter function)")

(defconstant +command-format-mapping+
  (make-hash-table :test #'string=)
  "Hash table containing mapping between command pseudo-formatting
and (FORMAT formatting + getter function")


@export-class
(defclass file-candidate ()
  ((source :accessor file-candidate-source :initarg :source
           :documentation "Source file (file to copy)")
   (target :accessor file-candidate-target :initarg :target
           :documentation "Target file (file to copy to)")
   (timestamp :accessor file-candidate-timestamp :initarg :timestamp
           :documentation "Timestamp of the source file")
   (comment :accessor file-candidate-comment :initarg :comment :initform ""
           :documentation "Comments, i.e. if the similar file already exists"))
  (:documentation "File Candidate is a struct holding information about
the input and output file name as well as the source file timestamp"))

(defmethod print-object ((self file-candidate) out)
  "Print overload for FILE-CANDIDATE class"
  (print-unreadable-object (self out :type t)
    (format out "~%   Source: ~s" (file-candidate-source self))
    (format out "~%   Target: ~s" (file-candidate-target self))
    (format out "~%   Comment: ~s" (file-candidate-comment self))))


;; fill the +timestamp-format-mapping+ and +command-format-mapping+
(eval-when (:compile-toplevel :load-toplevel)
  (setf (gethash "{YYYY}" +timestamp-format-mapping+)
        (cons "~4,'0d" #'datetime-year)
        (gethash "{MM}" +timestamp-format-mapping+)
        (cons "~2,'0d" #'datetime-month)
        (gethash "{MONTH}" +timestamp-format-mapping+)
        (cons "~a" #'datetime-string-month)
        (gethash "{MON}" +timestamp-format-mapping+)
        (cons "~a" (lambda (x) (datetime-string-month x :short t)))
        (gethash "{МЕСЯЦ}" +timestamp-format-mapping+)
        (cons "~a" (lambda (x) (datetime-string-month x :locale :ru)))
        (gethash "{МЕС}" +timestamp-format-mapping+)
        (cons "~a" (lambda (x) (datetime-string-month x :short t :locale :ru)))
        (gethash "{DD}" +timestamp-format-mapping+)
        (cons "~2,'0d" #'datetime-date)
        (gethash "{hh}" +timestamp-format-mapping+)
        (cons "~2,'0d" #'datetime-hour)
        (gethash "{mm}" +timestamp-format-mapping+)
        (cons "~2,'0d" #'datetime-minute)
        (gethash "{ss}" +timestamp-format-mapping+)
        (cons "~2,'0d" #'datetime-second))
  (setf (gethash "{SOURCE}" +command-format-mapping+)
        (cons "~s" (compose #'namestring #'file-candidate-source))
        (gethash "{TARGET}" +command-format-mapping+)
        (cons "~s" (compose #'namestring #'file-candidate-target))))
        



@export
(defclass renamer () ((source-path :initarg :source-path
                                   :documentation "Source directory to copy files from")
                      (destination-path :initarg :destination-path
                                        :documentation "Destination directory used as a base to copy files to")
                      (filemasks :initform nil :initarg :filemasks
                                 :documentation "File masks. In constructor one provides a string like \"*.jpg, *.png\" and it is converted to the list of regexps matching those filemasks")
                      (pattern :initform nil :initarg :pattern
                               :documentation "Renaming pattern. Example: \"{YYYY}-{MM}-{DD}/Photo-{hh}_{mm}.jpg\". If extension provided, use this extension, otherwise if no extension provided or it is a wildcard .* use original extensions")
                      (use-exif :initform nil :initarg :use-exif
                                :documentation "Boolean flag specifying if we need to try to extract information from EXIF. It takes little longer and not needed for example for movies")
                      (recursive :initform nil :initarg :recursive
                                 :documentation "Boolean flag specifying if we need to descend to subdirectories to collect list of files")
                      (checksums :initform (make-hash-table :test #'equal)
                                 :documentation "Cache of calculated checksums"))
  (:documentation "Renamer class encapsulates all the necessary information
needed for collecting files for copying/processing and creates a list
of candidates for copy/process"))


(defmethod initialize-instance :after ((self renamer) &key)
  "Constructor for RENAMER class"
  (with-slots (source-path destination-path filemasks) self
    ;; process paths
    (setf source-path (truename source-path))
    (setf destination-path (truename destination-path))
    ;; process filemasks
    (if (and filemasks (= (length filemasks) 0))
        (setf filemasks nil)
        (setf filemasks (mapcar (compose 
                                 #'ppcre:create-scanner
                                 #'wildcard-to-regex
                                 (curry #'string-trim " "))
                                (lw:split-sequence "," filemasks))))))


(defun format-timestamp-string (pattern ts)
  "Creates a formatted string from given pattern and timestamp.
Formatting arguments:
{YYYY}  - year
{MM}    - month (1-based)
{DD}    - day of month (1-based)
{Month} - literal month name (i.e. \"January\")
{hh}    - hour
{mm}    - minute
{ss}    - second

Example:
=> (format-timestamp-string \"{YYYY}-{MM}-{DD}/Photo-{hh}_{mm}.JPG\" (make-datetime-from-string \"2011:01:02 13:28:33333\"))
\"2011-01-02/Photo-13_28.JPG\""
  (format-string pattern ts +timestamp-format-mapping+))


(defun timestamp-based-filename (filename timestamp pattern)
  "Constructs the new filename relative path based on a file TIMESTAMP
using PATTERN. See FORMAT-TIMESTAMP-STRING for pattern description.

Example:
=> (timestamp-based-filename \"~/Sources/lisp/README.txt\" (make-datetime-from-string \"2011:01:02 13:28:33333\") \"{YYYY}-{MM}-{DD}/Photo-{hh}_{mm}.JPG\")
#P\"2011-01-02/Photo-13_28.JPG\""
  (let* ((ext (pathname-type filename))
         (new-name (format-timestamp-string pattern timestamp))
         (new-ext (pathname-type pattern))
         (new-dir (pathname-directory new-name))
         (new-basename (pathname-name new-name)))
    ;; test if we shall keep the old extension
    (if (or (not new-ext) ;; new not specified in patterns
            (eql new-ext :WILD) ;; new is a .*
            (string= (string-upcase ext) (string-upcase new-ext))) ;; same
        (make-pathname :directory new-dir :name new-basename :type ext)
        (make-pathname :directory new-dir :name new-basename :type new-ext))))


(defmethod construct-target-filename ((self renamer) input-filename)
  "By given INPUT-FILENAME construct
the full path to the file renamed after its timestamp using pattern specified
in the class instance.
If the :use-exif flag is set in the class instance and the file is JPEG,
try to get the EXIF information first for timestamp."
  (with-slots (destination-path pattern use-exif) self
    (let* ((ext (string-upcase (pathname-type input-filename)))
           (timestamp (or (and (equal ext "JPG")
                               use-exif
                               (make-datetime-from-exif input-filename))
                          (make-datetime-from-file input-filename)))
           (fname (timestamp-based-filename input-filename
                                            timestamp
                                            pattern)))
      (values 
       (fad:merge-pathnames-as-file
        (fad:pathname-as-directory destination-path) fname)
       timestamp))))


(defun integer-format (number digits)
  "Convert NUMBER to string with at least DIGITS digits.
Examples:

(integer-format 11 1)
                           
\"11\"
MEDIAIMPORT> (integer-format 11 2)
                           
\"11\"
MEDIAIMPORT> (integer-format 11 3)
                           
\"011\""
  (let ((fmt
         (format nil "~~~d,'0d" digits)))
    (format nil fmt number)))


(defun bump-file-name (filename &optional new-version)
  "Returns a bumped file name by given FILENAME. Bumped means
added '-version' to the end of the filename (before extension).

If NEW-VERSION provided use this instead of increasing original.
Examples:

=> (bump-file-name \"myfile.txt\")
#P\"myfile-1.txt\"

=> (bump-file-name \"myfile-1.txt\")
#P\"myfile-2.txt\"

=> (bump-file-name \"myfile-10.txt\")
#P\"myfile-11.txt\"

=> (bump-file-name \"myfile-10.txt\" 2)
#P\"myfile-02.txt\"

=> (bump-file-name \"myfile-10.txt\" 100)
#P\"myfile-100.txt\""
  (let* ((dir (pathname-directory filename)) ; directory
         (basename (pathname-name filename)) ; filename w/o extension
         (ext (pathname-type filename))      ; extension
         ;; possible numeric trailer like for "img10-1.jpg" it will be "1"
         (trailer (car (ppcre:all-matches-as-strings "-(\\d+$)" basename)))
         ;; number of digits in the new trailer. either 1 or as in old trailer
         (digits (if trailer (1- (length trailer)) 1))
         ;; version bump:
         (bump (cond (new-version new-version) ;; either provided
                     ;; or if trailer found, increase it
                     (trailer (1+ (parse-integer (subseq trailer 1))))
                     ;; otherwise just 1
                     (t 1)))
         (new-trailer (concatenate 'string "-"
                                   (integer-format bump digits)))
         (new-name (if trailer
                       (ppcre:regex-replace "-(\\d+$)" basename new-trailer)
                       (concatenate 'string basename new-trailer))))
    (make-pathname :directory dir :name new-name :type ext)))


(defun get-maximum-file-version (filenames)
  "By given the FILENAMES - list of file names of the same filemask,
like
 '(\"/Users/username/2/2016-03-13/Photo-18_36-1.jpg\"
  \"/Users/username/2/2016-03-13/Photo-18_36-2.jpg\"
  \"/Users/username/2/2016-03-13/Photo-18_36-3.jpg\"
  \"/Users/username/2/2016-03-13/Photo-18_36-4.jpg\"
  \"/Users/username/2/2016-03-13/Photo-18_36-5.jpg\"
  \"/Users/username/2/2016-03-13/Photo-18_36-6.jpg\"
  \"/Users/username/2/2016-03-13/Photo-18_36.jpg\")
return the maximum of versions or nil:
  6"
  (let ((trailers
         ;; remove "-" and convert to integers
         (mapcar (compose #'parse-integer (lambda (x) (subseq x 1)))
                 ;; remove nulls
                 (remove-if #'null
                            ;; parse pathnames extracting -number like -1
                            (mapcar
                             (compose #'car
                                      (curry #'ppcre:all-matches-as-strings "-(\\d+$)")
                                      #'pathname-name)
                             filenames)))))
    (if (not trailers)
        nil
        (apply #'max trailers))))
                   

(defun regexp-bumped-files (filename)
  "Create a regular expression based on filename to identify the
files with the same name as well as 'bumped' versions (ending
with -1, -2 etc.

Example:

=> (regexp-bumped-files \"myfile.txt\")
\"myfile(-(\\d+))?.(?i)txt$\"

=> (remove-if-not
 (lambda (x)
   (ppcre:all-matches \"myfile(-(\\d+))?.(?i)txt$\" x))
 '(\"myfile.txt\"
   \"one.txt\"
   \"myfile-2.txt\"
   \"myfile-10.txt\"
   \"myfile-07.TXT\"
   \"some-othermyfile.txt\"))
(\"myfile.txt\" \"myfile-2.txt\" \"myfile-10.txt\" \"myfile-07.TXT\")"
  (let ((dir (pathname-directory filename))
        (basename (pathname-name filename))
        (ext (pathname-type filename)))
    (concatenate 'string (if dir "/" "^")
                 basename "(-(\\d+))?"
                 (when ext (concatenate 'string ".(?i)" ext))
                 "$")))


(defun find-similar-files (filename)
  "Using the FILENAME try to create a list of similar files from
the directory where the FILENAME is located. It could be the same
file or bumped files based on FILENAME"
  (when-let (dir
             (fad:directory-exists-p
              (make-pathname :directory (pathname-directory filename))))
    (let ((files (mapcar #'namestring (fad:list-directory dir)))
          (regex (regexp-bumped-files filename)))
      (remove-if-not (lambda (x)
                       (ppcre:scan regex x))
                     files))))


(defmethod create-potential-file-candidates ((self renamer))
  "Create a preliminary list of file candidates.
This function iterates over all files, selecting those matching filemasks
and prepare a target name based on timestamp/etc information."
  (with-slots (source-path filemasks recursive) self
    ;; predicate which identifies if the filename is acceptable,
    ;; i.e. complies to any of file masks
    (flet ((acceptable-file (fname)
             (let ((short-name (file-namestring fname)))
               (some (lambda (x) (ppcre:scan x short-name)) filemasks))))
      (let (fnames)
        ;; collect list of all filenames into the fnames list
        (if recursive
            (fad:walk-directory source-path (lambda (x) (push x fnames)))
            (setf fnames (remove-if #'fad:directory-pathname-p (fad:list-directory source-path))))
        ;; finally remove not acceptable files (reversing back since we've pushed
        ;; files into the fnames list and create file-candidate for every file name
        (mapcar (lambda (x)
                  (multiple-value-bind (fname ts)
                      (construct-target-filename self x)
                    (make-instance 'file-candidate
                                   :source x
                                   :target fname
                                   :timestamp ts)))
                (nreverse
                 (if filemasks 
                     (remove-if-not #'acceptable-file fnames)
                     fnames)))))))


(defmethod verify-against-existing ((self renamer) candidates &key progress-fun)
  "Process the list of CANDIDATES and try to find existing files.
If existing files are in place AND are the same, set the candidate name as nil.
Otherwise try to bump the file name until no file with the same name exists.
After this operation CANDIDATES will contain targets either nil or non-existing
file names."
  (with-slots (checksums) self
    (let ((progress 0))
      ;; 1. Remove those candidates for which the target is already exists and
      ;;    the same
      (dolist (cand candidates)
        (when progress-fun (funcall progress-fun (incf progress)))
        (let ((target (file-candidate-target cand)))
          ;; find the list of similar files
          (when-let (similar (find-similar-files target))
            ;; some existing files are similar. Try to find those who are the same
            (if-let (found (some (lambda (x) (and (check-if-equal
                                                   (file-candidate-source cand)
                                                   x
                                                   checksums) x))
                                 similar))
                ;; found, clean the target and set appropriate comment
                (setf (file-candidate-target cand) nil
                      (file-candidate-comment cand)
                      (concatenate 'string string.same-as- found))
              ;; all existing are not the same as our target. Bump it then!
              (setf (file-candidate-target cand)
                    (bump-file-name target (1+ (get-maximum-file-version similar))))))))
      candidates)))
              

(defmethod bump-similar-candidates ((self renamer) candidates)
  "For each candidate if there is another candidates with the same name, bump
all of them"
  ;; first of all filter out candidates which found existing
  (let* ((new-candidates (delete-if (compose #'null #'file-candidate-target) (copy-list candidates))))
    (with-slots (checksums) self
      ;; the algorithm is the following:
      ;; 1. take the list of candidates
      (loop while new-candidates
            do
            ;; 2. pop the first candidate in the list            
            (let* ((next (pop new-candidates))
                   (target (file-candidate-target next))
                   (version (or (get-maximum-file-version (list target)) 0)))
              ;; 3. split remaining candidates to 2 groups:
              ;; with the same name and with different names
              ;; the group of remaining candidates with names != our
              ;; popped candidate name is the new list of candidates
              (multiple-value-bind (similar others) 
                  (partition new-candidates
                             (lambda (x)
                               (equalp (file-candidate-target x)
                                       target)))
                ;; 4. bump all candidates with the same name, by
                ;;    just increasing their versions
                (loop for cand in similar
                      for i from (1+ version) to (+ version (length similar))
                      do
                      (setf (file-candidate-target cand)
                            (bump-file-name (file-candidate-target cand) i)))
                (setf new-candidates others))))))
  candidates)


@export
(defmethod create-list-of-candidates ((self renamer) &key total-fun progress-fun)
  "Creates a final list of candidates. The main function of the renamer class,
its external interface.
TOTAL-FUN is a callback which will receive a list of items in progress-bar.
PROGRESS-FUN is a callback which will receive a current progress up
to the value received in TOTAL-FUN."
  ;; construct initial list of candidates
  ;; this operation could probably be lengthy, but since we don't
  ;; know yet the number of files to process, we cannot report
  ;; the progress
  (let ((candidates (create-potential-file-candidates self)))
    ;; report total number of items in progress bar
    ;; it is the number of files + 10% (reserved for progress of the bumping function)
    (when total-fun (funcall total-fun (+ (length candidates)
                                          (ceiling (/ (length candidates) 10.0)))))
    (bump-similar-candidates self
                             (verify-against-existing self candidates :progress-fun progress-fun))))


(defun check-if-equal (filename1 filename2 &optional checksum-hash)
  "Test if 2 files are equal.
1. First verify their sizes;
2. If sizes are the same, verify first 8kb of contents
3. If the 1st 8kb are the same, compare checksums.
If CHECKSUM-HASH table provided, try to lookup the checksum in
this table first and add if not found"
  (flet ((get-and-cache-checksum (fname)
           (if (not checksum-hash) (ironclad:digest-file :sha1 fname)
               (if-let ((cached (gethash fname checksum-hash)))
                   cached
                 (setf (gethash fname checksum-hash)
                       (ironclad:digest-file :sha1 fname))))))
    ;; first check file sizes
    (and (= (file-size filename1) (file-size filename2))
         ;; next check first 8k
         (equalp (read-header filename1 8192)
                 (read-header filename2 8192))
         ;; and after that we have to check checksum
         (let ((cs1 (get-and-cache-checksum filename1))
               (cs2 (get-and-cache-checksum filename1)))
           (equalp cs1 cs2)))))


(defun copy-file (from to)
  "Copy file FROM to the file TO overwriting it if exists"
  #-:lispworks
  (fad:copy-file from to :overwrite t)
  ;; only starting from 6.1
  #+:lispworks
  (lw:copy-file from to))


@export
(defun copy-files (file-candidates &key callback delete-original)
  "Copy files from array FILE-CANDIDATES of type file-candidate.
CALLBACK could be nil; if not nil, CALLBACK is a function which
is called every time file copied.
CALLBACK is a function of 2 arguments: index of the element in the
FILE-CANDIDATES array and a string error-text if an error happened.
In case of success 2nd argument is nil.
DELETE-ORIGINAL if t remove the original file"
  ;;
  ;; error occured while copying
  ;; CONDITIONS:FILE-STREAM-ERROR occured, arguments : (:ERRNO 57 :READ "copying to" :STREAM #P"/Volumes/storage-1/Media/Photo Archive/Mobile photos Alexey/2015-07-11/Video-15_56.MOV")
  (map-iota
   (lambda (i)
     (let* ((cand (aref file-candidates i))
            (from (file-candidate-source cand))
            (to (file-candidate-target cand))
            (result
             ;; result will contain either a nil or a error message
             (when to
                 (handler-case
                     (progn
                       (ensure-directories-exist (fad:pathname-directory-pathname to))
                       (copy-file from to)
                       nil)
                   (error (err)
                     (format nil "~a" err))))))
       ;; in case of success remove original
       (when (and delete-original (not result))
         (delete-file (file-candidate-source cand)))
       ;; if callback function is provided, call it
       (when callback
         (funcall callback i result))))
     (length file-candidates)))


(defun format-command-string (pattern cand)
  "Creates a formatted string from given pattern and candidate.
Formatting arguments:
{SOURCE} - source file
{TARGET} - target file
Example:
=> "
  (format-string pattern cand +command-format-mapping+))


@export
(defun validate-command-string (pattern)
  "Validate the PATTERN for command to be applied to file candidates.
Will return VALUES (result, error-text), where RESULT is t if the
{SOURCE} template pattern is found, and (nil, errortext) otherwise"
  (let ((matches (mappings-in-format-string pattern +command-format-mapping+)))
    (if (not (member "{SOURCE}" matches :test #'string=))
        (values nil string.source-not-provided)
        t)))

@export
(defun apply-command-to-files (file-candidates command-pattern
                                               &key
                                               callback stream script delete-original)
  "Applies the command-patten to all candidates from the array FILE-CANDIDATES
CALLBACK could be nil; if not nil, CALLBACK is a function which
is called every time file copied.
CALLBACK is a function of 2 arguments: index of the element in the
FILE-CANDIDATES array and a string error-text if an error happened.
In case of success 2nd argument is nil.
STREAM is a stream to redirect output to
DELETE-ORIGINAL if t remove the original file"
  (when script
    (format stream "#!/bin/sh~%"))
  (map-iota
   (lambda (i)
     (let* ((cand (aref file-candidates i))
            (result 0)
            (command ""))
       (when (and stream (file-candidate-target cand))
         (setf command (format-command-string command-pattern cand)
               result (if (not script)
                          (system:call-system-showing-output
                           command
                           :output-stream stream
                           :prefix "")
                          (format stream "~a~%" command)))
         ;; in case of success and not script remove original
         (when (and delete-original (not result) (not script))
           (delete-file (file-candidate-source cand))))
       (when callback
         (funcall callback i (when (/= 0 result)
                               (format nil string.failed-fmt command))))))
   (length file-candidates)))

  

@export
(defun init()
  "Platform-dependent initialization"
  #+sbcl
  (setf SB-ALIEN::*DEFAULT-C-STRING-EXTERNAL-FORMAT* :UTF-8)
  #+lispworks
  (lw:set-default-character-element-type 'lw:bmp-char))


;;; Tests
;; (in-package :mediaimport.renamer)
;; (setf r (make-instance 'renamer :source-path "~/1" :destination-path "~/2" :filemasks "*.jpg" :new-extension "png" :prefix "Photo-" :recursive t))
;; (construct-target-filename * "~/1/12442783_1081637521900005_512987139_n.jpg")
;; (pprint (create-potential-file-candidates r))
;; (pprint (create-list-of-candidates r))


