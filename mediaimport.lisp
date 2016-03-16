;;;; mediaimport.lisp

(in-package #:mediaimport)

;;; "mediaimport" goes here. Hacks and glory await!

(defstruct file-candidate source target timestamp)

(defclass renamer () ((source-path :initarg :source-path)
                      (destination-path :initarg :destination-path)
                      (prefix :initform nil :initarg :prefix)
                      (extensions :initform nil :initarg :extensions)
                      (new-extension :initform nil :initarg :new-extension)))

(defmethod initialize-instance :after ((self renamer) &key)
  (with-slots (source-path destination-path) self
    (setf source-path (truename source-path))
    (setf destination-path (truename destination-path))))

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


(defun timestamp-based-filename (filename &key new-ext prefix)
  "TODO: this is outdated
Constructs the new filename relative path based on a file timestamp.
Example:
=> (timestamp-based-filename \"~/Sources/lisp/README.txt\")
\"2016-03-06/IMAGE_16-47.txt\""
  (let ((ext (or new-ext (pathname-type filename))))
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time (file-write-date filename))
      (values
       (with-output-to-string (s)
         (format s "~4,'0d-~2,'0d-~2,'0d/~@[~a~]~2,'0d_~2,'0d~@[.~a~]" year month date prefix hour minute ext)
         s)
         (list year month date hour minute second)))))

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
  (with-slots (destination-path new-extension prefix) self
    (multiple-value-bind (fname ts)
        (timestamp-based-filename input-filename :new-ext new-extension :prefix prefix)
      (values 
       (fad:merge-pathnames-as-file
        (fad:pathname-as-directory destination-path) fname)
       ts))))

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
  "TODO: this is outdated
  Traverse through the INPUT-DIR and returns a list of pairs:
    ((file1 . output-file1) ... (fileN . output-fileN))
of filenames in the INPUT-DIR mapped according the timestamp.
Here fileN - file name in the directory INPUT-DIR,
output-fileN - file name constructed according to the timestamp
of the fileN.

OUTPUT-DIR is the optional output directory
INPUT-EXT is the extension (or list of extensions) of files
to process
OUTPUT-EXT the extension to be used instead of original ones
RECURSIVIE if set the processing will be done recursively"
  (with-slots (source-path destination-path extensions new-extension) self
    (flet ((correct-extension (fname)
             (let ((ext (string-upcase (pathname-type fname))))
               (cond ((null extensions) t)
                     ((atom extensions) (string= (string-upcase extensions) ext))
                     ((consp extensions)
                      (find ext extensions :test (lambda (x y)
                                                  (string= (string-upcase y) x))))))))
      (let (fnames)
        (if recursive
            (fad:walk-directory source-path (lambda (x) (push x fnames)))
            (setf fnames (remove-if #'fad:directory-pathname-p (fad:list-directory source-path))))
        (mapcar (lambda (x)
                  (multiple-value-bind (fname ts)
                      (construct-target-filename self x)
                    (make-file-candidate
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
  ;; for each candidate
  ;; while target exists and not the same or there is another candidate
  ;; with the same name, bump
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
         (bump-similar-candidates
          (verify-against-existing
           (construct-target-filenames self :recursive recursive))))
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

(defun init()
  #+sbcl
  (setf SB-ALIEN::*DEFAULT-C-STRING-EXTERNAL-FORMAT* :UTF-8))



;;; Tests
;; (in-package :mediaimport)
;; (setf r (make-instance 'renamer :source-path "~/1" :destination-path "~/2" :new-extension "png"))

;; (construct-target-filename * "~/1/12442783_1081637521900005_512987139_n.jpg")

;; (construct-target-filenames r)


