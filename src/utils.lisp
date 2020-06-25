;;;; utils.lisp
(defpackage #:mediaimport.utils
  (:documentation "Utility functions used in all packages")
  (:use #:cl #:alexandria)
  (:export
   from
   interleave
   partition
   file-size
   read-header
   directory-exists-p
   ;; duplicate-finder class
   duplicate-finder
   duplicate-p
   ;; other
   wildcard-to-regex
   format-string
   mappings-in-format-string
   define-resource
   push-top
   md5string
   view-file))

   
(in-package #:mediaimport.utils)


(define-constant +regex-escape-chars+
  '(#\\
    #\*
    #\+
    #\?
    #\|
    #\{
    #\}
    #\[
    #\]
    #\(
    #\)
    #\^
    #\$
    #\.
    #\#
    #\Space)
  :test #'equal
  :documentation "List of special characters to be escaped in file mask")

(defmacro from (package import name &rest others)
  "Import symbol(s) NAME ... from the package PACKAGE.
Examples:
(from mediaimport.utils import interleave partition +regex-escape-chars+)
(from mediaimport.ui import save-edit-controls-history)
(from mediaimport.utils import *)
In the last example imports all the exported symbols from the package given."
  (unless (string-equal import 'import)
    (error "Unexpected keyword: expected IMPORT, got ~A" import))
  (let* ((pkg (string-upcase (symbol-name package))) ;; package name as a string
         (symbols ; symbols to be imported
          (if (and (not others) (string-equal name "*"))
              ;; if called like (from something import *)
              (let (symbols)
                (do-external-symbols (s pkg)
                  (push s symbols))
                symbols)
              ;; otherwise just arguments list
              (cons name others))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (progn
       ,@(mapcar (lambda (symb)
                   (let ((import-symbol (find-symbol (string-upcase (symbol-name symb)) pkg)))
                     `(shadowing-import ,(list 'quote import-symbol))))
                 symbols)))))

#+win32
(progn
  (fli:define-c-typedef fli-hwnd
    (:unsigned :long))
  (fli:define-foreign-function (shell-execute-w32 "ShellExecute" :dbcs)
      ((hwnd fli-hwnd)
       (lpop :pointer)
       (lpfile :pointer)
       (lpparams :pointer)
       (lpdir :pointer)
       (nshowcmd :int))
    :result-type :pointer)
  (defun shell-execute (operation file params dir showcmd)
    (let ((external-format (if (string= (software-type)
                                        "Windows NT")
                               :unicode
                               :ascii)))
      (flet ((strconv (arg)
               (fli:convert-to-foreign-string arg :external-format external-format :allow-null t)))
        (shell-execute-w32 0
                           (strconv operation)
                           (strconv file)
                           (strconv params)
                           (strconv dir)
                           showcmd)))))

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


(defun partition (seq predicate)
  "Split the SEQ of type list by PREDICATE, returning the VALUES,
where 1st value is the list of elements for which PREDICATE is true,
and 2nd is the list of elements for which PREDICATE is false."
  (let ((pos (remove-if-not predicate seq))
        (neg (delete-if predicate seq)))
    (values pos neg)))


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


(defun directory-exists-p (dirname)
  "Return T if DIRNAME specifies an existing directory.
Suppress all errors and returns NIL otherwise"
  (handler-case
      (fad:directory-exists-p dirname)
    (error (err) nil)))


(defclass duplicate-finder () ((items :initarg :items)
                               (key :initarg :key :initform #'identity)
                               (nonuniques-table :initform
                                                 (make-hash-table :test #'equal)))
  (:documentation "Find duplicates in the array/list. Creates a hash table with
frequencies of encountered items to use later to determine if the item has a
duplicate in the array/list"))
                               

(defmethod initialize-instance :after ((self duplicate-finder) &key)
  "Constructor. Create an internal hash-table containing frequencies of
occurences of items in the array/list"
  (with-slots (items key nonuniques-table) self
    (map nil (lambda (x)
            (let* ((arg (funcall key x))
                   (ht-value (gethash arg nonuniques-table)))
              (if (not ht-value)
                  (setf (gethash arg nonuniques-table) 1)
                  (incf (gethash arg nonuniques-table)))))
          items)))
   

(defmethod duplicate-p ((self duplicate-finder) arg)
  "Returns t if the ARG was encountered more than once"
  (with-slots (nonuniques-table) self
    (multiple-value-bind (value result)
        (gethash arg nonuniques-table)
      (values (if (not result) nil (> value 1)) result))))


(defun wildcard-to-regex (wildcard &key case-sensitive-p)
  "Convert file wildcards to regular expressions. By default the regular
expression is case insensitive. This is regulated by keyword argument
CASE-SENSITIVE-P
Example:
=> (mediaimport.utils:wildcard-to-regex \"Photo*.jpg\") 
\"(?i)^Photo.*\\\\.jpg$\""
  ;; special case: *.* means * in reality
  (if (string= wildcard "*.*")
      ".*"
      ;; otherwise do processing
      (let ((regex
             (make-array (+ 8 (length wildcard))
                         :element-type
                         #+lispworks 'lw:bmp-char
                         #-lispworks 'character
                         :fill-pointer 0
                         :adjustable t)))
        (unless case-sensitive-p
          (vector-push-extend #\( regex)
          (vector-push-extend #\? regex)
          (vector-push-extend #\i regex)
          (vector-push-extend #\) regex))
        (vector-push-extend #\^ regex)
        (loop for char across wildcard do
              (cond ((eq char #\*)
                     (progn
                       (vector-push-extend #\. regex)
                       (vector-push-extend #\* regex)))
                    ((eq char #\?)
                     (vector-push-extend #\. regex))
                    ((find char +regex-escape-chars+)
                     (progn
                       (vector-push-extend #\\ regex)
                       (vector-push-extend char regex)))
                    (t (vector-push-extend char regex))))
        (vector-push-extend #\$ regex)
        regex)))


(defun format-string (pattern object mappings)
  "Creates a formatted string from given PATTERN, OBJECT and MAPPINGS.
Here the PATTERN is any string containing formatting arguments.
The MAPPINGS is a hashtable with the formatting as a key and a pair
(real format function argument, getter).
Here the format function argument is what should be used to format
the value received from OBJECT by calling a getter."
  (let* ((regex
          (format nil "(~{~A~^|~})"
                  (hash-table-keys mappings)))
         (matches (ppcre:all-matches-as-strings regex pattern))
         (result-list nil)
         (new-format-string (copy-seq pattern)))
    ;; create a format string replacing the templates like "{yyyy}" with
    ;; corresponding formatting options
    (maphash (lambda (key val)
               (setf new-format-string (ppcre:regex-replace-all key new-format-string (car val))))
             mappings)
    ;; collect all values in the correct order
    (dolist (key matches)
      (when-let (found (gethash key mappings))
        (push (funcall (cdr found) object) result-list)))
    (apply (curry #'format nil new-format-string) (nreverse result-list))))
  

(defun mappings-in-format-string (pattern mappings)
  "Returns a list of mappings from MAPPINGS hash table found in string PATTERN"
  (let* ((regex
          (format nil "(~{~A~^|~})"
                  (hash-table-keys mappings)))
         (matches (ppcre:all-matches-as-strings regex pattern)))
    matches))


(defmacro define-resource (name &body string-list)
  "Helper to declare and export constant with a given prefix.
NAME is a prefix,
STRING-LIST is a list of conses: constant name and constant value.
Example:
(define-resource string
  (to . \"To: \") 
  (from . \"From: \")
  (message . \"hello\"))

will generate the following code:
(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (PROGN (EXPORT 'STRING.TO) (DEFCONSTANT STRING.TO \"To: \"))
  (PROGN (EXPORT 'STRING.FROM) (DEFCONSTANT STRING.FROM \"From: \"))
  (PROGN (EXPORT 'STRING.MESSAGE) (DEFCONSTANT STRING.MESSAGE \"hello\")))

and the constants will be accessible like string.to etc.
"
  (declare (ignore lambda-list))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (lambda (string-item)
                 (let ((res-name
                        (intern
                         (concatenate 'string
                                      (symbol-name name)
                                      "."
                                      (symbol-name (car string-item))))))
                   `(progn
                      (export ',(or res-name '(nil)))
                      (defparameter ,res-name ,(cdr string-item)))))
               string-list)))

(defun push-top (elt lst &key (test #'eql))
  "Destructively push the element ELT to the top of the list LST, removing all
elements ELT in the LST, keeping the topmost element unique.
Example:
=> (let ((lst (list \"aa\" \"bb\" \"cc\" \"dd\")))
     (setf lst (push-top \"bb\" lst :test #'string-equal))
     lst)
(\"bb\" \"aa\" \"cc\" \"dd\")"
  (deletef lst elt :test test)
  (push elt lst))


(defun md5string (str)
  "Return a string representing MD5 hash of the input string STR"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5
                             (babel:string-to-octets str))))


(defun string-to-base-string (str)
  (loop with len = (length str)
        with result = (make-string len :element-type 'base-char)
        for n across (babel:string-to-octets str :encoding :utf-8)
        for i below len 
        do (setf (schar result i) (code-char n))
        finally (return result)))


(defun view-file (filename)
  #+win32
  (shell-execute "open" filename nil nil 1)
  #+cocoa
  ;; this function implements the following from Cocoa:
  ;; [[NSWorkspace sharedWorkspace] openFile:path];
  (objc:invoke (objc:invoke "NSWorkspace" "sharedWorkspace") "openFile:" filename)
  #+linux
  (sys:call-system (list "/usr/bin/xdg-open" (string-to-base-string filename)) :wait nil))

