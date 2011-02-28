(in-package :quickhoney.config)

;; URL für BASE HREFs
(defparameter *website-url* "http://quickhoney.com")

;; this is kind of a dirty hack to get the root of the thirdparty directory
(defparameter *thirdparty-directory*
  (asdf:system-relative-pathname :cl-gd #p"../"))

(defparameter *root-directory* (asdf:system-relative-pathname :quickhoney #p"../"))

(defparameter *store-directory* (merge-pathnames #p"datastore/" *root-directory*))

(defparameter *website-directory* (probe-file (merge-pathnames #p"website/" *root-directory*)))

(defparameter *xml-catalog-directory* (asdf:system-relative-pathname :quickhoney #p"../libs/bknr-web/src/web/dtd/catalog.xml"))

(defparameter *webserver-port* 8083)

;; please define following variables in a separate file paypal-config.lisp
;; (defparameter *paypal-user* "")
;; (defparameter *paypal-password* "")
;; (defparameter *paypal-signature* "")
;; (defparameter *paypal-secure-merchant-id* "")
;; (defparameter *paypal-url* "https://api-3t.sandbox.paypal.com/nvp")

;; 3 days
(defparameter *product-validity-time* 3)
