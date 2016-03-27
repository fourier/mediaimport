;;; Automatically generated delivery script

(in-package "CL-USER")

(load-all-patches)

#-quicklisp
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; add Sources/ directory to quicklisp local directories
(push (truename "~/Sources/lisp") ql:*local-project-directories*)
(ql:register-local-projects)


;;; Load the application:

(ql:quickload :mediaimport)


;;; Load the exmaple file that defines WRITE-MACOS-APPLICATION-BUNDLE
;;; to create the bundle.
(compile-file (sys:example-file "configuration/macos-application-bundle.lisp") :load t)

(deliver 'mediaimport-ui:main 
         (when (save-argument-real-p)
                 (write-macos-application-bundle
                   "~/Sources/lisp/mediaimport/build/mediaimport"))
         5
         :interface :capi)

