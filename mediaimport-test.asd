#|
  This file is a part of mediaimport project.
  Copyright (c) 2017-2018 Alexey Veretennikov (alexey.veretennikov@gmail.com)

  Test package.
  prove-asdf shall be installed in advance, i.e. via

  (ql:quickload "prove-asdf")

  Its a known limitation of quicklisp, it does not
  automatically fetches systems during the loading
  of system definition.

  Usage:
  
  (ql:quickload :mediaimport-test)
  (asdf/operate:test-system :mediaimport)
|#

(in-package :cl-user)
(defpackage mediaimport-test-asd
  (:use :cl :asdf))

(in-package :mediaimport-test-asd)

(defsystem mediaimport-test
  :author "Alexey Veretennikov"
  :license "BSD"
  :description "Test system for mediaimport"
  :depends-on (:mediaimport
               :alexandria
               :cl-fad
               :prove
               :prove-asdf)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "t"
                :components
                ((:file "base")
                 (:test-file "utils-test"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))

