(in-package :cl-user)

(defpackage :quickhoney.config
  (:use :cl
	:cl-user)
  (:export #:*website-url*
	   #:*website-directory*
	   #:*webserver-port*
	   #:*store-directory*
     #:*xml-catalog-directory*
     #:*thirdparty-directory*
     #:*root-directory*
     ))

(defpackage :quickhoney.imageproc
  (:use :cl
	:cl-user
	:bknr.web
	:bknr.images
	:cl-gd
	:quickhoney.config)
  (:export))

(defpackage :shop
  (:use :cl
        :bknr.datastore
        :bknr.indices
        :bknr.user)
  (:export #:download-product
           #:emailable-product
           #:mailable-product
           #:product-stock-count
           #:shopping-cart
           #:put-to-cart
           #:insufficient-inventory
           #:product-already-in-shopping-cart
           #:fulfill))

(defpackage :quickhoney
  (:use :cl
	:cl-user
	:alexandria
	:cl-interpol
	:cl-ppcre
	:hunchentoot
	:bknr.utils
	:bknr.web
	:bknr.user
	:bknr.datastore
	:bknr.indices
	:bknr.images
	:bknr.rss
	:quickhoney.config
	:xhtml-generator)
  (:shadowing-import-from :cl-interpol #:quote-meta-chars)
  (:shadowing-import-from :alexandria #:array-index)
  (:export #:last-image-upload-timestamp
           #:make-image-link
           #:quickhoney-image
           #:quickhoney-image-client
           #:quickhoney-image-spider-keywords
           #:quickhoney-image-description
           #:startup
           #:quickhoney-image-category
           #:quickhoney-image-subcategory
           #:all-categories
           #:subcategories-of
           #:images-in-category))

(defpackage :quickhoney.tags
  (:use :cl
	:cl-user
	:bknr.web
	:xhtml-generator
	:bknr.utils
	:quickhoney
	:quickhoney.config)
  (:shadowing-import-from :cl-interpol #:quote-meta-chars)
  (:export #:client-selectbox))

(defpackage :twitter
  (:use :cl :bknr.datastore)
  (:export #:update-status))

(defpackage :pixel-pdf
  (:use :cl)
  (:export #:convert-image-file-to-pdf
           #:convert-store-image-to-pdf))

(defpackage :turtle
  (:use :cl)
  (:export #:pen-down
           #:pen-up
           #:move-to
           #:turn
           #:forward
           #:reset
           #:x
           #:y
           #:line-to #:set-rgb-fill))
