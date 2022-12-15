;;;; datetime.lisp
(defpackage #:mediaimport.datetime
  (:use :cl :alexandria)
  (:export
   make-datetime-from-string
   make-datetime-from-gps-timestamps
   make-datetime-from-file
   make-datetime-from-exif
   datetime-string-month
   ;; datetime structure
   datetime
   datetime-second
   datetime-minute
   datetime datetime-year
   datetime-month
   create-datetime
   datetime-hour
   datetime-date))
   
(in-package #:mediaimport.datetime)

(define-constant +months+
  #((:en "January"   :ru "Январь")
    (:en "February"  :ru "Февраль")
    (:en "March"     :ru "Март")
    (:en "April"     :ru "Апрель")
    (:en "May"       :ru "Май")
    (:en "June"      :ru "Июнь")
    (:en "July"      :ru "Июль")
    (:en "August"    :ru "Август")
    (:en "September" :ru "Сентябрь")
    (:en "October"   :ru "Октябрь")
    (:en "November"  :ru "Ноябрь")
    (:en "December"  :ru "Декабрь"))
  :test #'equalp)

(define-constant +invalid-exif-stream+
  "Invalid EXIF information"
  :test #'string=)

(define-constant +invalid-jpeg-stream+
  "Invalid JPEG file"
  :test #'string=)

(defstruct (datetime
            (:constructor create-datetime (year month date hour minute second))
            (:constructor))
  "Simple date/time structure"
  year month date hour minute second)


(defmethod print-object ((self datetime) out)
  "Print overload for DATETIME struct"
  (format out "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
          ;; year-month-day
          (datetime-year self) (datetime-month self) (datetime-date self)
          ;; hour:minute:second
          (datetime-hour self) (datetime-minute self) (datetime-second self)))


(defun make-datetime-from-string (str)
  "Create a datetime struct from the string like \"2011:01:02 13:28:10\".
Example:
=> (make-datetime-from-string \"2011:01:02 13:28:33333\")
#S(DATETIME :YEAR 2011 :MONTH 1 :DATE 2 :HOUR 13 :MINUTE 28 :SECOND 33333)"
  (flet ((sep-p (c) (or (char= c #\-) (char= c #\:))))
    (let ((parsed-numbers
           (mapcar #'parse-integer
                   (apply #'append
                          (mapcar (lambda (x)
                                    (split-sequence:split-sequence-if #'sep-p x))
                                  (split-sequence:split-sequence #\Space str))))))
      (apply #'create-datetime parsed-numbers))))


(defun make-datetime-from-gps-timestamps (gds gts)
  "Create a datetime struct from pair of EXIF GPS timestamp values
extracted with zpb-exif library, GPSDateStamp(GDS) and GPSTimeStamp(GTS).
The GPSDateStamp is in format like\"2015:06:09\"
The GPSTimeStamp is in format like #(18 29 299/10)"
  (let ((parsed-numbers
         (append (mapcar #'parse-integer (split-sequence:split-sequence #\: gds))
                 (mapcar #'truncate (map 'list #'identity  gts)))))
    (apply #'create-datetime parsed-numbers)))


(defun make-datetime-from-file (filename)
  "Create a datetime struct from the file timestamp"
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (file-write-date filename))
    (make-datetime :year year :month month :date date
                   :hour hour :minute minute :second second)))


(defun make-datetime-from-exif (filename)
  "Create a datetime struct from the EXIF information contianing in
the file FILENAME.
Returns values:
datetime error-description
If EXIF found: datetime nil
If no EXIF found: nil nil
If error while parsing: nil string"
  (handler-case
      (let* ((exif (zpb-exif:make-exif (truename filename)))
             (exif-ht (alist-hash-table (zpb-exif:exif-alist exif) :test #'equal)))
        (flet ((get-value (name)
                 (or (zpb-exif:exif-value name exif) (gethash name exif-ht))))
          (let ((dto (get-value "DateTimeOriginal"))
                ;; "2012:01:23 00:17:40"
                (dt (get-value "DateTime"))
                ;; "2012:01:23 00:17:40"
                (dtd (get-value "DateTimeDigitized"))
                ;; "2016:06:23 12:11:54"
                (gds (get-value "GPSDateStamp"))
                ;;"2015:06:09"
                (gts (get-value "GPSTimeStamp")))
            ;; #(18 29 299/10)
            ;; logic the flowing:
            (values
             (cond ((or dto dt dtd)
                    ;; if DateTimeOriginal or DateTime or DateTimeDigitized, use it
                    (make-datetime-from-string (or dto dt)))
                   ((and gds gts) ;; if both GPSDateStamp and GPSTimeStamp
                    (make-datetime-from-gps-timestamps gds gts)))
             nil))))
    (zpb-exif:invalid-exif-stream (err)
      (declare (ignore err))
      (values nil +invalid-exif-stream+))
    (zpb-exif:invalid-jpeg-stream (err)
      (declare (ignore err))
      (values nil +invalid-jpeg-stream+))))


(defun datetime-string-month (dt &key short (locale :en))
  "Return textual representation of the month"
  (let* ((month (datetime-month dt))
         (result 
          (getf (aref mediaimport.datetime::+months+ (1- month)) locale)))
    (if short (subseq result 0 3) result)))
        
