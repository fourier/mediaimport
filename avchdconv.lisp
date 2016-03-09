;;;; avchdconv.lisp

(in-package #:avchdconv)

;;; "avchdconv" goes here. Hacks and glory await!

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

;; (defun bump-file-name (filename)
;;   (if (fad:file-exists-p filename)
;;       (

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

(defun init()
  #+sbcl
  (setf SB-ALIEN::*DEFAULT-C-STRING-EXTERNAL-FORMAT* :UTF-8))


;;; Tests

;; (make-instance 'renamer :destination-path *destination-path* :new-extension "png")

;; (construct-target-filename * "~/1/crap.jpg")


