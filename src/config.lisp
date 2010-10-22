(in-package :quickhoney.config)

;; URL für BASE HREFs
(defparameter *website-url* "http://quickhoney.ruinwesen.com")

;; this is kind of a dirty hack to get the root of the thirdparty directory
(defparameter *thirdparty-directory*
  (asdf:system-relative-pathname :cl-gd #p"../"))

(defparameter *root-directory* (asdf:system-relative-pathname :quickhoney #p"../"))

(defparameter *store-directory* (merge-pathnames #p"datastore/" *root-directory*))

(defparameter *website-directory* (probe-file (merge-pathnames #p"website/" *root-directory*)))

(defparameter *xml-catalog-directory* (asdf:system-relative-pathname :quickhoney #p"../xml/"))

(defparameter *webserver-port* 8082)


;; paypal configuration, should be encrypted so that it doesn't lie around in the repository

(defparameter *paypal-user* "larve1_1284454514_biz_api1.bl0rg.net")

(defparameter *paypal-password* "1284454523")

(defparameter *paypal-signature* "AzfqBFUGNgH7udd-MtFLWWxWZWNgAPxKDWtYjO5EMo-JeQzKmQUEYPrW")

(defparameter *paypal-secure-merchant-id* "W7PDYJMZUSC5E")

;; 3 days
(defparameter *product-validity-time* 3)
