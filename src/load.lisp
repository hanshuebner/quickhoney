;; -*- Lisp -*-

(require 'asdf)

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

(setup-registry #P"../../")

(push :cl-gd-gif *features*)

(asdf:oos 'asdf:load-op :quickhoney)
(asdf:oos 'asdf:load-op :swank)

(swank::create-server :port 4085)

;; disable ldb
#+sbcl
(sb-alien:define-alien-routine ("disable_lossage_handler" disable-sbcl-ldb) sb-alien:void)
#+sbcl
(disable-sbcl-ldb)

#+cmu
(mp::startup-idle-and-top-level-loops)
