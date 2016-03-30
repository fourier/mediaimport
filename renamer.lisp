;;;; mediaimport.lisp
(defpackage #:mediaimport.renamer
  (:use #:cl #:cl-annot.class #:alexandria
   #:mediaimport.utils #:mediaimport.datetime))

(in-package #:mediaimport.renamer)
(annot:enable-annot-syntax)

;;; "mediaimport" goes here. Hacks and glory await!

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


@export
(defclass renamer () ((source-path :initarg :source-path)
                      (destination-path :initarg :destination-path)
                      (prefix :initform nil :initarg :prefix)
                      (extensions :initform nil :initarg :extensions)
                      (new-extension :initform nil :initarg :new-extension)
                      (use-exif :initform nil :initarg :use-exif)
                      (checksums :initform (make-hash-table :test #'equal))))

(defmethod initialize-instance :after ((self renamer) &key)
  (with-slots (source-path destination-path extensions new-extension) self
    ;; process paths
    (setf source-path (truename source-path))
    (setf destination-path (truename destination-path))
    ;; process extensions
    (cond ((and extensions (= (length extensions) 0))
           (setf extensions nil))
          ((and extensions (atom extensions))
           (setf extensions (mapcar (lambda (x)
                                      (string-upcase (string-trim " " x)))
                                    (lw:split-sequence "," extensions)))
           (when (= (length extensions) 1)
             (setf extensions (car extensions))))
          ((and extensions (listp extensions))
           (setf extensions (mapcar (lambda (x)
                                      (string-upcase (string-trim " " x)))
                                    extensions))))
    (setf new-extension (if (= (length new-extension) 0)
                            nil
                            (string-trim " " new-extension)))))

  

(defun timestamp-based-filename (filename timestamp
                                          &key
                                          new-ext
                                          prefix)
  "TODO: this is outdated
Constructs the new filename relative path based on a file timestamp.
Example:
=> (timestamp-based-filename \"~/Sources/lisp/README.txt\")
\"2016-03-06/IMAGE_16-47.txt\""
  (let ((ext (or new-ext (pathname-type filename))))
    (with-output-to-string (s)
         (format s "~4,'0d-~2,'0d-~2,'0d/~@[~a~]~2,'0d_~2,'0d~@[.~a~]"
                 (datetime-year timestamp)
                 (datetime-month timestamp)
                 (datetime-date timestamp)
                 prefix
                 (datetime-hour timestamp)
                 (datetime-minute timestamp)
                 ext)
         s)))


(defmethod construct-target-filename ((self renamer) input-filename)
  "TODO: this is outdated
By given INPUT-FILENAME and optionally OUTPUT-DIR construct
the full path to the file renamed after its timestamp.
Optional key EXT allows to select another file extension.
Example:
=> (construct-target-filename \"~/Sources/lisp/README.txt\")
#P\"/Volumes/storage/Video/From Camera/2016-03-06/16-47.txt\"

=> (construct-target-filename \"~/Sources/lisp/README.txt\" :output-dir \"/Users/username\" :ext \"mp4\")
#P\"/Users/username/2016-03-06/16-47.mp4\""
  (with-slots (destination-path new-extension prefix use-exif) self
    (let* ((ext (string-upcase (pathname-type input-filename)))
           (timestamp (or (and (equal ext "JPG")
                               use-exif
                               (make-datetime-from-exif input-filename))
                          (make-datetime-from-file input-filename)))
           (fname (timestamp-based-filename input-filename
                                            timestamp
                                            :new-ext new-extension
                                            :prefix prefix)))
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
         (with-output-to-string (s)
           (format s "~~~d,'0d" digits)
           s)))
    (with-output-to-string (s)
      (format s fmt number))))

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


(defmethod construct-target-filenames ((self renamer) &key recursive)
  "TODO: document it"
  (with-slots (source-path extensions) self
    (flet ((correct-extension (fname)
             (let ((ext (string-upcase (pathname-type fname))))
               (cond ((null extensions) t)
                     ((atom extensions) (string= extensions ext))
                     ((consp extensions)
                      (find ext extensions :test (lambda (x y)
                                                  (string= y x))))))))
      (let (fnames)
        (if recursive
            (fad:walk-directory source-path (lambda (x) (push x fnames)))
            (setf fnames (remove-if #'fad:directory-pathname-p (fad:list-directory source-path))))
        (mapcar (lambda (x)
                  (multiple-value-bind (fname ts)
                      (construct-target-filename self x)
                    (make-instance 'file-candidate
                     :source x
                     :target fname
                     :timestamp ts)))
                (nreverse
                 (if extensions 
                     (remove-if-not #'correct-extension fnames)
                     fnames)))))))

(defmethod verify-against-existing ((self renamer) candidates)
  "Process the list of CANDIDATES and try to find existing files.
If existing files are in place AND are the same, set the candidate name as nil.
Otherwise try to bump the file name until no file with the same name exists.
After this operation CANDIDATES will contain targets either nil or non-existing
file names."
  (with-slots (checksums) self
    ;; 1. Remove those candidates for which the target is already exists and
    ;;    the same
    (dolist (cand candidates)
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
                    (concatenate 'string "Same as: " found))
            ;; all existing are not the same as our target. Bump it then!
            (setf (file-candidate-target cand)
                  (bump-file-name target (1+ (get-maximum-file-version similar))))))))
    candidates))
              

(defmethod bump-similar-candidates ((self renamer) candidates)
  "For each candidate if there is another candidates with the same name, bump
all of them"
  (with-slots (checksums) self
    (let ((new-candidates (delete-if (compose #'null #'file-candidate-target) (copy-list candidates))))
      (loop while new-candidates
            do
            (let* ((next (pop new-candidates))
                   (target (file-candidate-target next))
                   (version (get-maximum-file-version (list target))))
              (multiple-value-bind (similar others) 
                  (partition new-candidates
                             (lambda (x)
                               (equalp (file-candidate-target x)
                                       target)))
                (loop for cand in similar
                      for i from (1+ version) to (+ version (length similar))
                      do
                      (setf (file-candidate-target cand)
                            (bump-file-name (file-candidate-target cand) i)))
                 (setf new-candidates others))))))
    candidates)

@export
(defmethod create-list-of-candidates ((self renamer) &key recursive)
  (let ((files
         (bump-similar-candidates self
          (verify-against-existing self
           (construct-target-filenames self :recursive recursive)))))
    files))

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
(defun copy-files (file-candidates &key callback)
  "Copy files from array FILE-CANDIDATES of type file-candidate.
CALLBACK could be nil; if not nil, CALLBACK is a function which
is called every time file copied.
CALLBACK is a function of 2 arguments: index of the element in the
FILE-CANDIDATES array and a string error-text if an error happened.
In case of success 2nd argument is nil."
  (map-iota
   (lambda (i)
     (let* ((cand (aref file-candidates i))
            (from (file-candidate-source cand))
            (to (file-candidate-target cand))
            (result
             ;; result will contain either a nil or a error message
             (if to
                 (handler-case
                     (progn 
                       (ensure-directories-exist (fad:pathname-directory-pathname to))
                       (copy-file from to)
                       nil)
                   (file-error (err)
                     (with-output-to-string (s)
                       (format s "~a" err))))
                 t)))
            ;; if callback function is provided, call it
       (when callback
         (funcall callback i result))))
     (length file-candidates)))
  
           

@export
(defun init()
  #+sbcl
  (setf SB-ALIEN::*DEFAULT-C-STRING-EXTERNAL-FORMAT* :UTF-8)
  #+lispworks
  (lw:set-default-character-element-type 'lw:bmp-char))


;;; Tests
;; (in-package :mediaimport.renamer)
;; (setf r (make-instance 'renamer :source-path "~/1" :destination-path "~/2" :extensions "jpg" :new-extension "png"))
;; (construct-target-filename * "~/1/12442783_1081637521900005_512987139_n.jpg")
;; (pprint (construct-target-filenames r))
;; (pprint (create-list-of-candidates r))

