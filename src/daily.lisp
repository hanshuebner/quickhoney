
(in-package :quickhoney)

(defun snapshot-store ()
  (format t "; snapshotting datastore~%")
  (snapshot)
  (format t "; done~%"))
