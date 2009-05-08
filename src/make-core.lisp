(in-package :cl-user)

(defparameter *src-directory* (make-pathname :directory (pathname-directory *load-truename*)))
(defparameter *thirdparty-directory* #p"home:bknr-svn/thirdparty/")
(defparameter *bknr-directory* #p"home:bknr-svn/bknr/")

#+(or) (load "../../thirdparty/mcclim/system.lisp")

(load (merge-pathnames "patches/patch-around-mop-cmucl19a.lisp" *bknr-directory*))
(load (merge-pathnames "asdf/asdf.lisp" *thirdparty-directory*))

(pushnew *src-directory* asdf:*central-registry* :test #'equal)

(defun make-wild-pathname (type directory)
  (merge-pathnames (make-pathname :type type
				  :directory '(:relative :wild-inferiors))
		   directory))

(defun setup-registry (directory-path)
  (format t "; adding components under ~A to asdf registry~%" directory-path)
  (mapc (lambda (asd-pathname)
          (pushnew (make-pathname :directory (pathname-directory asd-pathname))
                   asdf:*central-registry*
                   :test #'equal))
	(remove "asd" (directory (merge-pathnames #p"**/" directory-path))
		:test (complement #'equal)
		:key #'pathname-type)))

(setup-registry *thirdparty-directory*)
(setup-registry *bknr-directory*)

(ext:save-lisp "cmucl.core")
