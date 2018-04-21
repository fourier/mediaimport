;;;; generic-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :mediaimport)' in your Lisp.
;;

(in-package :cl-user)
(defpackage mediaimport.test.utils-test
  (:use :cl
   :alexandria
        :mediaimport.test.base
   :mediaimport.utils
        :prove))

(in-package :mediaimport.test.utils-test)

(plan nil)

(subtest "Test interleave"
  (test-input interleave '((1 2 3) (-1 -2 -3)) 
              '(1 -1 2 -2 3 -3)))

(finalize)
