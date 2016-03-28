;;;; mediaimport.lisp
(defpackage #:mediaimport.utils
  (:use #:cl #:cl-annot.class))

(in-package #:mediaimport.utils)
(annot:enable-annot-syntax)


@export-structure
(defstruct (datetime (:constructor create-datetime (year month date hour minute second))
                     ;(:constructor)
                     )
  "Simple date/time structure"
  year month date hour minute second)

@export
(defmethod print-object ((self datetime) out)
  "Print overload for DATETIME struct"
  (format out "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
          ;; year-month-day
          (datetime-year self) (datetime-month self) (datetime-date self)
          ;; hour:minute:second
          (datetime-hour self) (datetime-minute self) (datetime-second self)))

@export
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

@export
(defun make-datetime-from-gps-timestamps (gds gts)
  "Create a datetime struct from pair of EXIF GPS timestamp values
extracted with zpb-exif library, GPSDateStamp(GDS) and GPSTimeStamp(GTS).
The GPSDateStamp is in format like\"2015:06:09\"
The GPSTimeStamp is in format like #(18 29 299/10)"
  (let ((parsed-numbers
         (append (mapcar #'parse-integer (lw:split-sequence ":" gds))
                 (mapcar #'truncate (map 'list #'identity  gts)))))
    (apply #'create-datetime parsed-numbers)))


@export
(defun make-datetime-from-file (filename)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (file-write-date filename))
    (make-datetime :year year :month month :date date
                   :hour hour :minute minute :second second)))

@export
(defun make-datetime-from-exif (filename)
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


@export
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

@export
(defun file-size (filename)
  "Return the size of the file with the name FILENAME in bytes"
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (file-length in)))

@export
(defun read-header (filename size)
  "Read SIZE bytes from the file FILENAME. If the file size is less than SIZE,
read up to the size of file"
  (let ((elt-type '(unsigned-byte 8)))
    (with-open-file (in filename :element-type elt-type)
      (let* ((fsize (file-length in))
             (buffer (make-array (min size fsize) :element-type elt-type)))
        (read-sequence buffer in)
        buffer))))



@export
(defclass duplicate-finder () ((items :initarg :items)
                               (key :initarg :key :initform #'identity)
                               (nonuniques-table :initform
                                                 (make-hash-table :test #'equal))))
                               

(defmethod initialize-instance :after ((self duplicate-finder) &key)
  (with-slots (items key nonuniques-table) self
    (map nil (lambda (x)
            (let* ((arg (funcall key x))
                   (ht-value (gethash arg nonuniques-table)))
              (if (not ht-value)
                  (setf (gethash arg nonuniques-table) 1)
                  (incf (gethash arg nonuniques-table)))))
          items)))
   

@export
(defmethod duplicate-p ((self duplicate-finder) arg)
  (with-slots (nonuniques-table) self
    (multiple-value-bind (value result)
        (gethash arg nonuniques-table)
      (values (if (not result) nil (> value 1)) result))))

