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

(define-persistent-class paypal-action-log ()
  ((result :read
	   :documentation "Complete PAYPAL request result for logging purposes")
   (token :read
	  :index-type hash-index
	  :index-reader paypal-actions-for-token
	  :documentation "PAYPAL transaction token")))

(define-persistent-class paypal-product-transaction ()
  ((product :read
	    :index-type hash-index
	    :index-reader paypal-transactions-for-product)
   (token :read
	  :index-type string-unique-index
	  :index-reader paypal-transaction-for-token
	  :index-values all-paypal-transactions)
   (status :update :initform :ongoing
	   :documentation "Can be either :ONGOING, :CANCELLED, :SUCCESSFUL")
   (creation-time :update :initform (get-universal-time))
   (buyer-info :update
	       :initform nil
	       :documentation "Paypal user information")))

(defmethod paypal-txn-valid-p ((txn paypal-product-transaction))
  (and (eql (paypal-product-transaction-status txn)
	    :successful)
       (not (paypal-txn-expired-p txn))))

(defmethod paypal-txn-expired-p ((txn paypal-product-transaction))
  (< (+ (paypal-product-transaction-creation-time txn)
	 (* 3 24 3600))
      (get-universal-time)))

(defclass json-paypal-checkout-handler (page-handler)
  ())

(defmethod handle ((handler json-paypal-checkout-handler))
  (with-json-response ()
    (with-query-params (price image color)
      (handler-case 
	  (let* ((id (parse-integer image))
		 (img (find-store-object id :class 'quickhoney-image))
		 (product (when img (quickhoney-image-pdf-product img))))
	    (unless (and img product)
	      (error "Could not find PDF for image with id ~A" image))
	    (multiple-value-bind (link res)
		(cl-paypal:make-express-checkout-url price (real-remote-addr)
						     :l_paymentrequest_0_name0 (store-image-name img)
						     :l_paymentrequest_0_number0 id
						     :l_paymentrequest_0_amt0 price
						     :l_paymentrequest_0_qt0 1
						     :paymentrequest_0_itemamt price
						     :hdrbordercolor color
						     :allownote 0
						     :noshipping 1
						     :hdrimg (format nil "http://quickhoney.ruinwesen.com/image/66899/thumbnail"))
	      
	      (let ((token (getf res :token)))
		;; log paypal action
		(make-instance 'paypal-action-log
			       :result res :token token)

		;; create product transaction
		(make-instance 'paypal-product-transaction
			       :product product
			       :token token
			       :status :ongoing)
		
		(json:encode-object-element
		 "paypalLink"
		 link)
		(json:encode-object-element
		 "buttonLink"
		 (format nil "https://fpdbs.sandbox.paypal.com/dynamicimageweb?cmd=_dynamic-image&pal=~A"
			 *paypal-secure-merchant-id*)))))

	(error (e)
	  (format t "error while paypal ~A~%" e)
	  (json:encode-object-element
	   "error"
	   (format nil "~A" e)))))))

(defclass paypal-success-handler (page-handler)
  ())

(defmethod handle ((handler paypal-success-handler))
  (let* ((token (get-parameter "token"))
	 (payerid (get-parameter "PayerID"))
	 (txn (paypal-transaction-for-token token)))
    (unless txn
      (error "Could not find paypal transaction for token ~A" token))
    (multiple-value-bind (result response)
	(cl-paypal:do-express-checkout token)
      (cl-paypal::unregister-transaction token)
      (make-instance 'paypal-action-log
		     :result result
		     :token token)
      (make-instance 'paypal-action-log
		     :result response
		     :token token)

      (with-transaction ()
	(setf (paypal-product-transaction-creation-time txn) (get-universal-time)
	      (paypal-product-transaction-status txn) :successful))
      
      (setf *paypal-success-res* result
	    *paypal-success-response* response)
      (redirect (format nil "/#paypal/~A" token)))))

(defclass paypal-cancel-handler (page-handler)
  ())

(defmethod handle ((handler paypal-cancel-handler))
  (let* ((token (get-parameter "token"))
	 (payerid (get-parameter "PayerID"))
	 (txn (paypal-transaction-for-token token)))
    (unless txn
      (error "Could not find paypal transaction for token ~A" token))

    (let ((response (cl-paypal:get-express-checkout-info token)))
      (cl-paypal::unregister-transaction token)
      (make-instance 'paypal-action-log
		     :result response
		     :token token)
      
      (with-transaction ()
	(setf (paypal-product-transaction-creation-time txn) (get-universal-time)
	      (paypal-product-transaction-status txn) :cancelled))
      
      (setf *paypal-cancel-res* response)
      (redirect (format nil "/#paypal/~A" token)))))

(defun transaction-with-download-id (id)
  (paypal-transaction-for-token id))

(defclass quickhoney-client-pdf-handler (object-handler)
  ()
  (:default-initargs :query-function #'transaction-with-download-id
    :object-class 'paypal-product-transaction))

(defmethod handle-object ((handler quickhoney-client-pdf-handler) txn)
  (cond ((paypal-txn-valid-p txn)
	 (let ((product (paypal-product-transaction-product txn)))
	   (handle-if-modified-since (blob-timestamp product))
	   (setf (header-out :last-modified) (rfc-1123-date (blob-timestamp product)))
	   (with-http-response (:content-type "application/pdf")
	     (setf (header-out :content-length) (blob-size product))
	     (with-open-file (blob-data (blob-pathname product) :element-type '(unsigned-byte 8))
	       (copy-stream blob-data (send-headers) :element-type '(unsigned-byte 8))))))
	(t (redirect (format nil "/#paypal/~A" (store-object-id txn))))))
	
(defclass json-paypal-transaction-info-handler (object-handler)
  ()
  (:default-initargs :query-function #'transaction-with-download-id
    :object-class 'paypal-product-transaction))

(defmethod handle-object ((handler json-paypal-transaction-info-handler) txn)
  (with-json-response ()
    (with-slots (product token status creation-time) txn
      (json:encode-object-element "token" token)
      (json:encode-object-element "status" status)
      (json:encode-object-element "valid" (paypal-txn-valid-p txn))
      (json:encode-object-element "expired" (paypal-txn-expired-p txn))
      (json:with-object-element ("image")
	(image-to-json (quickhoney-product-image product))))))