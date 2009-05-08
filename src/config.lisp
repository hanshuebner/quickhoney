(in-package :quickhoney.config)

;; URL für BASE HREFs
(defparameter *website-url* "http://quickhoney.com")

(defparameter *root-directory* (merge-pathnames #p"../" (make-pathname :name nil :type nil :version nil :defaults *load-pathname*)))

(defparameter *store-directory* (merge-pathnames #p"datastore/" *root-directory*))

(defparameter *website-directory* (probe-file (merge-pathnames #p"website/" *root-directory*)))

(defparameter *webserver-port* 8081)
