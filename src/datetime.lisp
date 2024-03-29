;;;; datetime.lisp
(defpackage #:mediaimport.datetime
  (:use :cl :alexandria)
  (:export
   make-datetime-from-string
   make-datetime-from-gps-timestamps
   make-datetime-from-file
   make-datetime-from-exif
   make-datetime-from-filename-regexp   
   make-datetime-from-filename-pattern
   datetime-string-month
   ;; datetime structure
   datetime
   datetime-second
   datetime-minute
   datetime-hour
   datetime-year
   datetime-month
   datetime-date   
   create-datetime
   ;; aux funtions patterns-related
   datetime-regexp-from-filename-pattern
   patterns-from-string
   keyword-for-pattern))
   
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

(declaim (ftype (function (t) t) month-name-to-number))

(defparameter +datetime-pattern-mapping+
  (alist-hash-table
   `(("{YYYY}"   .   (:re "((?:19|20)[0-9][0-9])" :convert parse-integer :kw :year))
     ("{MM}"     .   (:re "((?:0[1-9])|(?:1[0-2]))" :convert parse-integer :kw :month))
     ("{DD}"     .   (:re "((?:0[1-9])|(?:1[0-9])|(?:2[0-9])|(?:3[0-1]))" :convert parse-integer :kw :date))
     ("{hh}"     .   (:re "((?:0[0-9])|(?:1[0-9])|(?:2[0-3]))" :convert parse-integer :kw :hour))
     ("{mm}"     .   (:re "([0-5][0-9])" :convert parse-integer :kw :minute))
     ("{ss}"     .   (:re "([0-5][0-9])" :convert parse-integer :kw :second))
     ("{MONTH}"  .   (:re ,(format nil "(~{~a~^|~})" (loop for x across +months+ collect (getf x :en)))
                      :convert ,(lambda (x) (month-name-to-number x :short nil :locale :en))
                      :kw :month))
     ("{MON}"    .   (:re ,(format nil "(~{~a~^|~})" (loop for x across +months+ collect (subseq (getf x :en) 0 3)))
                      :convert ,(lambda (x) (month-name-to-number x :short t :locale :en))
                      :kw :month))
     ("{МЕСЯЦ}" . (:re ,(format nil "(~{~a~^|~})" (loop for x across +months+ collect (getf x :ru)))
                              :convert ,(lambda (x) (month-name-to-number x :short t :locale :en))
                              :kw :month))
     ("{МЕС}"     . (:re,(format nil "(~{~a~^|~})" (loop for x across +months+ collect (subseq (getf x :ru) 0 3)))
                              :convert ,(lambda (x) (month-name-to-number x :short t :locale :en))
                              :kw :month)))
   :test #'equal))


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

(let (months-en mons-en months-ru mons-ru)
  (defun month-name-to-number (month-name &key short (locale :en))
    "Return the month number 1..12 from [long or short] month name"
    (when (not months-en)
      (setf months-en (make-hash-table :test #'string=)
            mons-en (make-hash-table :test #'string=)
            months-ru (make-hash-table :test #'string=)
            mons-ru (make-hash-table :test #'string=))
      (loop with len = (length +months+)
            for i below len
            for month = (aref +months+ i)
            for mon-en = (getf month :en)
            for mon-ru = (getf month :ru)
            do
            (setf (gethash mon-en months-en) (1+ i)
                  (gethash mon-ru months-ru) (1+ i)
                  (gethash (subseq mon-ru 0 3) mons-en) (1+ i)
                  (gethash (subseq mon-en 0 3) mons-en) (1+ i))))
    (gethash month-name    
             (cond ((and (not short) (eql locale :en)) months-en)
                   ((and short (eql locale :en) mons-en))
                   ((and (not short) (eql locale :ru)) months-ru)
                   ((and short (eql locale :ru) mons-ru))))))

(defun get-month-string (month-num &key short (locale :en))
  "Get the [localized][short] month name string.
Agrument month-num should be in range 1..12"
  (let ((result 
         (getf (aref mediaimport.datetime::+months+ (1- month-num)) locale)))
    (if short (subseq result 0 3) result)))


(defun datetime-string-month (dt &key short (locale :en))
  "Return textual representation of the month"
  (get-month-string (datetime-month dt) :short short :locale locale))

(defun preprocess-filename-pattern (filename-pattern)
  "Preprocess the filename pattern, escaping the dot, adding
end of line marker, expanding wildcards, adding optional
versioning of the file name"
  (destructuring-bind (name . ext) (ppath:splitext filename-pattern)
    ;; process separately name and extension
    (flet ((expand-wildcards (str)
                            (mediaimport.utils:wildcard-to-regex
                                    str :case-sensitive-p t
                              :escape-special-p nil
                              :start-end-markers-p nil)))
      (setf name (expand-wildcards name)
            ext (expand-wildcards ext)))
    ;; add regexp part for versions
    (setf name (lw:string-append name "(?:-\\d+)?"))
    (lw:string-append "(?i)" name "\\" ext "$")))

(defun datetime-regexp-from-pattern (pattern)
  "Creates a regexp for parsing the string according to the pattern.
Example:
=> (datetime-regexp-from-pattern \"{YYYY}-{MM}-{DD}-{hh}_{mm}\")
\"((?:19|20)[0-9][0-9])-((?:0[1-9])|(?:1[0-2]))-((?:0[1-9])|(?:1[0-9])|(?:2[0-9])|(?:3[0-1]))-((?:0[0-9])|(?:1[0-9])|(?:2[0-3]))_([0-5][0-9])\"
"
  ;; get the list of patterns
  (let ((patterns (patterns-from-string pattern)))
    ;; at least one pattern found, good
    (when patterns
      ;; "massage" the string. add end-line matcher, escape dot
      (let ((result-regexp (copy-array pattern)))
        (dolist (pattern patterns)
          (let ((mapping (gethash pattern +datetime-pattern-mapping+)))
            (setf result-regexp
                  (ppcre:regex-replace-all pattern
                                           result-regexp 
                                           (getf mapping :re)))))
        result-regexp))))

(defun datetime-regexp-from-filename-pattern (pattern)
  "Creates a regexp for parsing the filename string according to the pattern.
Example:
=> (datetime-regexp-from-filename-pattern \"Photo-{YYYY}-{MM}.jpg\")
\"(?i)Photo-((?:19|20)[0-9][0-9])-((?:0[1-9])|(?:1[0-2]))(?:-\\d+)?\\.jpg$\"
"
   (datetime-regexp-from-pattern (preprocess-filename-pattern pattern)))


(defun datetime-from-filename (parsed patterns)
  "Create a datetime from the given array of strings and the list of corresponding patterns.
First argument is an array, second is a list, of strings both.
Example:
=> (datetime-from-filename #(\"01\" \"02\" \"03\") '(\"{hh}\" \"{mm}\" \"{ss}\"))
0nil-nil-nil 01:02:03
"
  ;; create hash table with keywords (key) and parsed values (val)
  (let* ((tbl 
          (loop
           with patterns-table = (make-hash-table :test #'equal)
           for val across parsed
           for pat in patterns
           for mapping = (gethash pat +datetime-pattern-mapping+)
           for parsed-val = (funcall (getf mapping :convert) val)
           do 
           (setf (gethash (getf mapping :kw) patterns-table) parsed-val)
           finally (return patterns-table)))
         ;; collect all keys (keywords) and values (numbers) into the flat list
         (args
          (loop for k being each hash-key of tbl using (hash-value v) collect k collect v)))
    ;; and use the flat list as the arguments to constructor
    (apply #'make-datetime args)))

(defun make-datetime-from-filename-regexp (regexp patterns filename)
  "Create a datetime object from the filename with a given pattern.
Example:
=> (make-datetime-from-filename-regexp
    (datetime-regexp-from-filename-pattern
    \"Photo-{YYYY}-{MON}/{MONTH}.jpg\")
    (patterns-from-string
     \"Photo-{YYYY}-{MON}/{MONTH}.jpg\")
    \"Photos/PHoto-2020-Feb/December.jpg\")
2020-12-nil nil:nil:nil"
    (multiple-value-bind (match values)
        (ppcre:scan-to-strings regexp filename)
      (declare (ignore match))
      (when values
        ;; amount of strings shall be equal to amount of patterns
        (assert (= (length patterns) (length values)))
        (datetime-from-filename values patterns))))

(defun make-datetime-from-filename-pattern (pattern filename)
  "Create a datetime object from the filename with a given pattern.
Example:
=> (make-datetime-from-filename-pattern
    \"Photo-{YYYY}-{MON}/{MONTH}.jpg\"
    \"Photos/PHoto-2020-Feb/December.jpg\")
2020-12-nil nil:nil:nil"
  ;; TODO: Cache regexp and patterns
  (let ((regexp (datetime-regexp-from-filename-pattern pattern))
        (patterns (patterns-from-string pattern)))
    (make-datetime-from-filename-regexp regexp patterns filename)))

(defun patterns-from-string (pattern-string)
  "Extracts the recognized date/time patterns from the string.
Used to validate the input"
  (mediaimport.utils:mappings-in-format-string
   pattern-string +datetime-pattern-mapping+))


(defun keyword-for-pattern (pattern)
  "Return the keyword for a given text pattern.
Example:
=> (keyword-for-pattern \"{MON}\")
:month"
  (when-let (mapping (gethash pattern +datetime-pattern-mapping+))
    (getf mapping :kw)))
