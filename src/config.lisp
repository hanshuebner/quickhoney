(in-package :quickhoney.config)

;; URL für BASE HREFs
(defparameter *website-url* "http://quickhoney.com")

;; this is kind of a dirty hack to get the root of the thirdparty directory
(defparameter *thirdparty-directory*
  (asdf:system-relative-pathname :cl-gd #p"../"))

(defparameter *root-directory* (asdf:system-relative-pathname :quickhoney #p"../"))

(defparameter *store-directory* (merge-pathnames #p"datastore/" *root-directory*))

(defparameter *website-directory* (probe-file (merge-pathnames #p"website/" *root-directory*)))

(defparameter *xml-catalog-directory* (asdf:system-relative-pathname :quickhoney #p"../xml/"))

(defparameter *webserver-port* 8081)
