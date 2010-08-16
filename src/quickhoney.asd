;;;; -*- Mode: LISP -*-

(in-package :cl-user)

(defpackage :quickhoney.system
  (:use :cl :asdf))

(in-package :quickhoney.system)

(defsystem :quickhoney
  :name "worldpay test"
  :author "Hans Huebner <hans@huebner.org>"
  :version "0"
  :maintainer "Hans Huebner <hans@huebner.org>"
  :licence "BSD"
  :description "worldpay test web server"
  :long-description ""

  :depends-on (:cl-interpol
	       :cl-ppcre
	       :cxml
	       :cl-mime
               :drakma
	       :bknr.web
	       :bknr.datastore
	       :bknr.modules
	       :cl-gd
               :unit-test
               :yason
               #+(or) :cl-pdf)

  :components ((:file "packages")
	       (:file "config" :depends-on ("packages"))
	       (:file "image" :depends-on ("config"))
               (:file "news" :depends-on ("image"))
	       (:file "layout" :depends-on ("config"))
	       (:file "imageproc" :depends-on ("config"))
               (:file "twitter" :depends-on ("packages"))
	       (:file "handlers" :depends-on ("layout" "config" "image" "news"))
	       (:file "tags" :depends-on ("image"))
	       (:file "webserver" :depends-on ("handlers"))
	       (:file "daily" :depends-on ("config"))

               #+(or) (:file "turtle" :depends-on ("packages"))
               #+(or) (:file "pixel-pdf" :depends-on ("turtle"))

               (:file "money" :depends-on ("packages"))
               (:file "shop" :depends-on ("money"))
               (:file "quickhoney-shop" :depends-on ("shop"))

	       (:file "init" :depends-on ("webserver" "daily"))))
