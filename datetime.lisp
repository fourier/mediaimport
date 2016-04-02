;;;; datetime.lisp
(defpackage #:mediaimport.datetime
  (:use #:cl #:cl-annot.class))

(in-package #:mediaimport.datetime)
(annot:enable-annot-syntax)


@export-structure
(defstruct (datetime
            (:constructor create-datetime (year month date hour minute second))
            (:constructor))
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
  "Create a datetime struct from the file timestamp"
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (file-write-date filename))
    (make-datetime :year year :month month :date date
                   :hour hour :minute minute :second second)))

@export
(defun make-datetime-from-exif (filename)
  "Create a datetime struct from the EXIF information contianing in
the file FILENAME. If no EXIF found returns nil"
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
(defun datetime-localized-month (dt)
  (let ((month (datetime-month dt)))
    (case month
      (1 "January")
      (2 "February")
      (3 "March")
      (4 "April")
      (5 "May")
      (6 "June")
      (7 "July")
      (8 "August")
      (9 "September")
      (10 "October")
      (11 "November")
      (12 "December")
      (otherwise "Unknown"))))