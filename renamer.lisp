;;;; mediaimport.lisp
(defpackage #:mediaimport.renamer
  (:use #:cl #:cl-annot.class #:mediaimport.utils #:mediaimport.datetime))

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
           :documentation "Timestamp of the source file"))
  (:documentation "File Candidate is a struct holding information about
the input and output file name as well as the source file timestamp"))

(defmethod print-object ((self file-candidate) out)
  "Print overload for FILE-CANDIDATE class"
  (print-unreadable-object (self out :type t)
    (format out "~%   Source: ~s" (file-candidate-source self))
    (format out "~%   Target: ~s" (file-candidate-target self))))


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

(defun bump-file-name (filename)
  (let* ((dir (pathname-directory filename)) ; directory
         (basename (pathname-name filename)) ; filename w/o extension
         (ext (pathname-type filename))      ; extension
         ;; possible numeric trailer like for "img10-1.jgp" it will be "1"
         (trailer (car (ppcre:all-matches-as-strings "-(\\d+$)" basename)))
         ;; number of digits in the new trailer. either 1 or as in old trailer
         (digits (if trailer (1- (length trailer)) 1))
         ;; version bump, if trailer found, increase it, otherwise just 1
         (bump (if trailer (1+ (parse-integer (subseq trailer 1))) 1))
         (new-trailer (concatenate 'string "-"
                                   (integer-format bump digits)))
         (new-name (if trailer
                       (ppcre:regex-replace "-(\\d+$)" basename new-trailer)
                       (concatenate 'string basename new-trailer))))
    (make-pathname :directory dir :name new-name :type ext)))
                     
                                                    
    
(defmethod construct-target-filenames ((self renamer) &key recursive)
  "TODO: document it"
  (with-slots (source-path destination-path extensions new-extension) self
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

(defun verify-against-existing (candidates)
  "Process the list of candidates and try to find existing files.
If existing files are in place AND are the same, set the candidate name as nil.
Otherwise try to bump the file name until no file with the same name exists"
  ;; 1. Remove those candidates for which the target is already exists and
  ;;    the same
  (let ((fresh-new
         (remove-if (lambda (cand) (and (fad:file-exists-p (file-candidate-target cand))
                                        (check-if-equal (file-candidate-source cand)
                                                        (file-candidate-target cand))))
                    candidates)))
    fresh-new))

(defun bump-similar-candidates (candidates)
"For each candidate
while target exists and not the same or there is another candidate
with the same name, bump"
  (let ((new-candidates (copy-list candidates)))
    (dolist (c new-candidates)
      (let ((from (file-candidate-source c)))
        (loop while (or (and (fad:file-exists-p (file-candidate-target c))
                             (not (check-if-equal from (file-candidate-target c))))
                        (find-if (lambda (x)
                                   (and 
                                   (string-equal (namestring (file-candidate-target x)) (namestring (file-candidate-target c)))
                                   (not (string-equal (namestring (file-candidate-source x)) (namestring (file-candidate-source c))))))
                                 new-candidates))
           do
             (let ((new-version (bump-file-name (file-candidate-target c))))
               (setf (file-candidate-target c) new-version)))))
    new-candidates))


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
  (alexandria:map-iota
   (lambda (i)
     (let* ((cand (aref file-candidates i))
            (from (file-candidate-source cand))
            (to (file-candidate-target cand))
            (result
             ;; result will contain either a nil or a error message
             (handler-case
                 (progn 
                   (ensure-directories-exist (fad:pathname-directory-pathname to))
                   (copy-file from to)
                   nil)
               (file-error (err)
                 (with-output-to-string (s)
                   (format s "~a" err))))))
       ;; if callback function is provided, call it
       (when callback
         (funcall callback i result))))
     (length file-candidates)))

@export
(defmethod create-list-of-candidates ((self renamer) &key recursive)
  (let ((files
         (bump-similar-candidates
          (verify-against-existing
           (construct-target-filenames self :recursive recursive)))))
    files))
  
           

(defun check-if-equal (filename1 filename2)
  ;; first check file sizes
  (and (= (file-size filename1) (file-size filename2))
       ;; next check first 8k
       (equalp (read-header filename1 8192)
               (read-header filename2 8192))
       ;; and after that we have to check checksum
       (let ((cs1 (ironclad:digest-file :sha1 filename1))
             (cs2 (ironclad:digest-file :sha1 filename2)))
         (equalp cs1 cs2))))

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


