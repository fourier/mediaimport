;;;; mediaimport.lisp

(in-package #:mediaimport)

;;; "mediaimport" goes here. Hacks and glory await!

(defvar *destination-path* "/Volumes/storage/Video/From\ Camera")
(defvar *source-path* "/Users/alexeyv/1")

(defclass renamer () ((source-path :initarg :source-path)
                      (destination-path :initarg :destination-path)
                      (prefix :initform nil :initarg :prefix)
                      (extensions :initform nil :initarg :extensions)
                      (new-extension :initform nil :initarg :new-extension)))

(defmethod initialize-instance :after ((self renamer) &key)
  (with-slots (source-path destination-path) self
    (setf source-path (truename source-path))
    (setf destination-path (truename destination-path))))

(defun timestamp-based-filename (filename &key new-ext prefix)
  "TODO: this is outdated
Constructs the new filename relative path based on a file timestamp.
Example:
=> (timestamp-based-filename \"~/Sources/lisp/README.txt\")
\"2016-03-06/IMAGE_16-47.txt\""
  (let ((ext (or new-ext (pathname-type filename))))
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time (file-write-date filename))
      (declare (ignore second))
      (with-output-to-string (s)
        (format s "~4,'0d-~2,'0d-~2,'0d/~@[~a~]~2,'0d-~2,'0d~@[.~a~]" year month date prefix hour minute ext)))))

(defmethod construct-target-filename ((self renamer) input-filename)
  "TODO: this is outdated
By given INPUT-FILENAME and optionally OUTPUT-DIR construct
the full path to the file renamed after its timestamp.
Optional key EXT allows to select another file extension.
Example:
=> (construct-target-filename \"~/Sources/lisp/README.txt\")
#P\"/Volumes/storage/Video/From Camera/2016-03-06/16-47.txt\"

=> (construct-target-filename \"~/Sources/lisp/README.txt\" :output-dir \"/Users/alexeyv\" :ext \"mp4\")
#P\"/Users/alexeyv/2016-03-06/16-47.mp4\""
  (with-slots (destination-path new-extension prefix) self
    (fad:merge-pathnames-as-file
     (fad:pathname-as-directory destination-path)
     (timestamp-based-filename input-filename :new-ext new-extension :prefix prefix))))

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
                  (cons x 
                        (construct-target-filename self x)))
                (nreverse
                 (if extensions 
                     (remove-if-not #'correct-extension fnames)
                     fnames)))))))

(defmethod rename-files ((self renamer) &key recursive)
  (let ((files (construct-target-filenames self :recursive recursive)))
    (format t "The list of files to be renamed:~%")
    (dolist (f files)
      (format t "~a =>~%~a~%" (car f) (cdr f)))
    (format t "~%~% Renaming ...~%")
    (dolist (f files)    
      (ensure-directories-exist (fad:pathname-directory-pathname (cdr f)))
      (rename-file (car f) (cdr f)))
    (format t "~% Done.~%")))

(defun check-if-equal (filename1 filename2)
  (let ((cs1 (ironclad:digest-file :sha1 filename1))
        (cs2 (ironclad:digest-file :sha1 filename2)))
    (equalp cs1 cs2)))

(defun init()
  #+sbcl
  (setf SB-ALIEN::*DEFAULT-C-STRING-EXTERNAL-FORMAT* :UTF-8))



;;; Tests

;; (make-instance 'renamer :destination-path *destination-path* :new-extension "png")

;; (construct-target-filename * "~/1/crap.jpg")


