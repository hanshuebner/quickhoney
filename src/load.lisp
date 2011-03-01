;; -*- Lisp -*-

(require 'asdf)

(ql:quickload '(:bknr.datastore
                :cl-fad
                :cl-mime
                :cl-pdf
                :cl-ppcre
                :cxml
                :drakma
                :md5
                :parenscript
                :rfc2388
                :stem
                :unit-test
                :xhtmlgen
                :yason
		:trivial-backtrace))

(defun setup-registry (directory-path)
  (format t "; adding components under ~A to asdf registry~%" directory-path)
  (mapc (lambda (asd-pathname)
	  (pushnew (make-pathname :name nil
				  :type nil
				  :version nil 
				  :defaults asd-pathname)
		   asdf:*central-registry*
		   :test #'equal))
	(directory (merge-pathnames #p"**/*.asd" directory-path))))

(setup-registry #P"../")

(push :cl-gd-gif *features*)

(asdf:oos 'asdf:load-op :quickhoney)

(asdf:oos 'asdf:load-op :swank)
(swank::create-server :port 4087 :dont-close t)

;; disable ldb
#+sbcl
(sb-alien:define-alien-routine ("disable_lossage_handler" disable-sbcl-ldb) sb-alien:void)
#+sbcl
(disable-sbcl-ldb)

#+cmu
(mp::startup-idle-and-top-level-loops)
