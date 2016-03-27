;;;; mediaimport.lisp
(defpackage #:mediaimport
  (:use #:cl #:cl-annot.class #:mediaimport-utils))

(in-package #:mediaimport)
(annot:enable-annot-syntax)

;;; "mediaimport" goes here. Hacks and glory await!

@export
@export-accessors
(defclass file-candidate ()
  ((source :accessor file-candidate-source :initarg :source)
   (target :accessor file-candidate-target :initarg :target)
   (timestamp :accessor file-candidate-timestamp :initarg :timestamp)))

(defmethod print-object ((self file-candidate) out)
  (print-unreadable-object (self out :type t)
    (format out "~%   Source: ~s" (file-candidate-source self))
    (format out "~%   Target: ~s" (file-candidate-target self))))


(defstruct (datetime (:constructor create-datetime (year month date hour minute second))
                     (:constructor)) year month date hour minute second)


@export
(defclass renamer () ((source-path :initarg :source-path)
                      (destination-path :initarg :destination-path)
                      (prefix :initform nil :initarg :prefix)
                      (extensions :initform nil :initarg :extensions)
                      (new-extension :initform nil :initarg :new-extension)
                      (use-exif :initform nil :initarg :use-exif)))

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

(defun file-size (filename)
  "Return the size of the file with the name FILENAME in bytes"
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (file-length in)))

(defun read-header (filename size)
  "Read SIZE bytes from the file FILENAME. If the file size is less than SIZE,
read up to the size of file"
  (let ((elt-type '(unsigned-byte 8)))
    (with-open-file (in filename :element-type elt-type)
      (let* ((fsize (file-length in))
             (buffer (make-array (min size fsize) :element-type elt-type)))
        (read-sequence buffer in)
        buffer))))

(defun interleave (list1 list2)
  "Interleaves 2 lists.
Example:
=> (interleave '(1 2 3) '(-1 -2 -3))
(1 -1 2 -2 3 -3)"
  (let ((parity t)
        (result (copy-list list1)))
    (merge 'list result list2
           (lambda (x y)
             (declare (ignore x) (ignore y))
             (setf parity (not parity))))
    result))

(defun make-datetime-from-string (str)
  "Create a datetime struct from the string like \"2011:01:02 13:28:10\".
Example:
=> (make-datetime-from-string \"2011:01:02 13:28:33333\")
#S(DATETIME :YEAR 2011 :MONTH 1 :DATE 2 :HOUR 13 :MINUTE 28 :SECOND 33333)"
  (let ((parsed-numbers
         (mapcar #'parse-integer (apply #'append
                                        (mapcar (lambda (x)
                                                  (lw:split-sequence ":" x))
                                                (lw:split-sequence " " str))))))
    (apply #'create-datetime parsed-numbers)))

(defun make-datetime-from-gps-timestamps (gds gts)
  "Create a datetime struct from pair of EXIF GPS timestamp values
extracted with zpb-exif library, GPSDateStamp(GDS) and GPSTimeStamp(GTS).
The GPSDateStamp is in format like\"2015:06:09\"
The GPSTimeStamp is in format like #(18 29 299/10)"
  (let ((parsed-numbers
         (append (mapcar #'parse-integer (lw:split-sequence ":" gds))
                 (mapcar #'truncate (map 'list #'identity  gts)))))
    (apply #'create-datetime parsed-numbers)))
  
(defun timestamp-from-file (filename)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (file-write-date filename))
    (make-datetime :year year :month month :date date
                   :hour hour :minute minute :second second)))


(defun timestamp-from-exif (filename)
  (handler-case
      (let* ((exif (zpb-exif:make-exif (truename filename)))
             (dto (zpb-exif:exif-value :DateTimeOriginal exif))
             ;; "2012:01:23 00:17:40"
             (dt (zpb-exif:exif-value :DateTime exif))
             ;; "2012:01:23 00:17:40"
             (gds (zpb-exif:exif-value :GPSDateStamp exif))
             ;;"2015:06:09"
             (gts (zpb-exif:exif-value :GPSTimeStamp exif)))
        ;; #(18 29 299/10)
        ;; logic the flowing:
        (cond ((or dto dt) ;; if DateTimeOriginal or DateTime, use it
               (make-datetime-from-string (or dto dt)))
              ((and gds gts) ;; if both GPSDateStamp and GPSTimeStamp
               (make-datetime-from-gps-timestamps gds gts))))
    (zpb-exif:invalid-exif-stream (err) nil)))


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
                               (timestamp-from-exif input-filename))
                          (timestamp-from-file input-filename)))
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


(defun yes-no (&optional prompt)
  (let (answer)
    (loop while (not (and answer
                          (or (char-equal (char-upcase answer) #\Y)
                              (char-equal (char-upcase answer) #\N))))
          do
          (if prompt
              (format *standard-output* "~%~a~%" prompt)
              (format *standard-output* "~%[y]es/[n]o ?~%"))
          (setq answer (read-char)))
    (char-equal (char-upcase answer) #\Y)))

(defun copy-file (from to)
  #-:lispworks
  (fad:copy-file from to :overwrite t)
  ;; only starting from 6.1
  #+:lispworks
  (lw:copy-file from to)
  (values))

(defmethod merge-files ((self renamer) &key delete-original recursive) 
  (let ((files
         (create-list-of-candidates self :recursive recursive))
        (merge-fun (if delete-original #'rename-file #'copy-file)))
    (format t "The list of files to be renamed:~%")
    (dolist (f files)
      (format t "~a =>~%~a~%" (file-candidate-source f) (file-candidate-target f)))
    (when (yes-no (if delete-original "Rename files ?" "Copy files ?"))
      (format t "~%~% Processing ...~%")
      (dolist (f files)
        (let ((from (file-candidate-source f))
              (to (file-candidate-target f)))
          (ensure-directories-exist (fad:pathname-directory-pathname to))
          (funcall merge-fun from to)))
      (format t "~% Done.~%"))))

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
;; (in-package :mediaimport)
;; (setf r (make-instance 'renamer :source-path "~/1" :destination-path "~/2" :extensions "jpg" :new-extension "png"))

;; (construct-target-filename * "~/1/12442783_1081637521900005_512987139_n.jpg")

;; (pprint (construct-target-filenames r))


