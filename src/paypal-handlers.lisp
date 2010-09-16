(in-package :quickhoney)

(defparameter *paypal-sandbox-api-url* "https://api-3t.sandbox.paypal.com/nvp")

(defun paypal-init (&key (user *paypal-user*)
		    (password *paypal-password*)
		    (signature *paypal-signature*))
  (cl-paypal:init
   *paypal-sandbox-api-url*
   user password signature
   (format nil "~A/paypal-success" quickhoney.config:*website-url*)
   (format nil "~A/paypal-cancel" quickhoney.config:*website-url*)
   :useraction "commit"
   :currencycode "USD"))

(defclass json-paypal-checkout-handler (page-handler)
  ())

(defmethod handle ((handler json-paypal-checkout-handler))
  (with-json-response ()
    (with-query-params (price image color)
      (handler-case 
	  (let* ((id (parse-integer image))
		 (img (find-store-object id :class 'quickhoney-image)))
	    (unless img
	      (error "Could not find image with id ~A" image))
	    (let ((link (cl-paypal:make-express-checkout-url
			 price (real-remote-addr)
			 :l_paymentrequest_0_name0 (store-image-name img)
			 :l_paymentrequest_0_number0 id
			 :l_paymentrequest_0_amt0 price
			 :l_paymentrequest_0_qt0 1
			 :paymentrequest_0_itemamt price
			 :hdrbordercolor color
			 :hdrimg (format nil "~A/image/66899/thumbnail,,750,90" *website-url*)
			 )))
	      (json:with-object-element ("queryResult")
		(json:with-object ()
		  (json:encode-object-element
		   "paypalLink"
		   link)
		  (json:encode-object-element
		   "buttonLink"
		   (format nil "https://fpdbs.sandbox.paypal.com/dynamicimageweb?cmd=_dynamic-image&pal=~A"
			   *paypal-secure-merchant-id*))))))
	(error (e)
	  (format t "error while paypal ~A~%" e)
	  (json:with-object-element ("queryError")
	    (json:with-object ()
	      (json:encode-object-element
	       "errorString"
	       (format nil "~A" e)))))))))

(defclass paypal-success-handler (page-handler)
  ())

(defvar *paypal-req* nil)

(defmethod handle ((handler paypal-success-handler))
  (setf *paypal-req* *request*))

(defclass paypal-cancel-handler (page-handler)
  ())

(defmethod handle ((handler paypal-cancel-handler))
  (setf *paypal-req-cancel* *request*))

