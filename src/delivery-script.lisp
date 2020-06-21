;;; Automatically generated delivery script

(in-package "CL-USER")

(load-all-patches)

#+MSWINDOWS (load "C:/apps/asdf/asdf.lisp")
#+cocoa (load "~/Sources/lisp/asdf.lisp")

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init #+:MSWINDOWS "C:/apps/quicklisp/setup.lisp"
					  #-:MSWINDOWS (merge-pathnames ".quicklisp/setup.lisp"
                                                                        (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; add Sources/ directory to quicklisp local directories
;; add Sources/ directory to quicklisp local directories
(push (pathname #+:MSWINDOWS "C:/Sources/lisp" #-:MSWINDOWS "~/Sources/lisp") ql:*local-project-directories*)
(ql:register-local-projects)

;; Extend the stack size to be able to load all dependencies
(hcl:extend-current-stack 400)

;;; Load the application:

(ql:quickload :mediaimport)


;;; Load the exmaple file that defines WRITE-MACOS-APPLICATION-BUNDLE
;;; to create the bundle.
#+cocoa
(compile-file (sys:example-file "configuration/macos-application-bundle.lisp") :load t)

(deliver 'mediaimport:main
         #+cocoa
         (create-macos-application-bundle
          "~/Sources/lisp/mediaimport/Media Import.app"
          ;; Do not copy file associations...
          :document-types nil
          ;; ...or CFBundleIdentifier from the LispWorks bundle
          :identifier "com.github.fourier.mediaimport")
         #+MSWINDOWS
         "C:/Sources/lisp/mediaimport/mediaimport.exe"
         4
         :interface :capi
         :keep-symbols '(mediaimport.ui::geometry-settings) ;; why doesn't work ?
         :startup-bitmap-file nil)

