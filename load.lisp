;; -*- Lisp -*-

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

(setup-registry #.(make-pathname :name nil :type nil :defaults (or *compile-file-pathname* *load-pathname*)))

#+sbcl
(sb-alien:alien-funcall
 (sb-alien:extern-alien "disable_lossage_handler" (function sb-alien:void)))

(ql:quickload :quickhoney)