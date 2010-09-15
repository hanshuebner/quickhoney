(in-package :quickhoney)

(define-persistent-class quickhoney-product ()
  ((price
    :update
    :type (or number null)
    :documentation
    "Product price in USD")
   (image
    :update
    :type (or quickhoney-image null)
    :index-type hash-index
    :index-reader products-for-image
    :documentation
    "Image that this product is related to, or nil for standalone products"))
  (:documentation
   "Mixin class for Quickhoney products, which are usually related to a QUICKHONEY-IMAGE"))

(define-persistent-class quickhoney-pdf-product (blob quickhoney-product)
  ()
  (:documentation "Download PDF Quickhoney product. The blob data is the PDF file"))

(define-persistent-class quickhoney-paypal-transaction ()
  ((product
    :read
    :type quickhoney-product
    :index-type hash-index
    :index-reader transactions-for-product
    :documentation
    "Product that this transaction relates to")
   (transaction-id :read)
   (date :read))
  (:documentation
   "Transaction class when a product is bought over paypal. Fields are speculative for now"))

(defmethod quickhoney-image-pdf-product-p ((image quickhoney-image))
  (let ((products (products-for-image image)))
    (not (emptyp (remove-if-not #'(lambda (product)
                                    (typep product 'quickhoney-pdf-product)) products)))))

(defmethod quickhoney-image-pdf-product ((image quickhoney-image))
  (let ((products (products-for-image image)))
    (first (remove-if-not #'(lambda (product)
                              (typep product 'quickhoney-pdf-product)) products))))

(defmethod quickhoney-image-pdf-price ((image quickhoney-image))
  (let ((product (quickhoney-image-pdf-product image)))
    (when product
      (quickhoney-product-price product))))

;;; shop edit handlers

(defclass upload-shop-handler (admin-only-handler edit-object-handler)
  ()
  (:default-initargs :object-class 'quickhoney-image))
.
(defmethod handle-object-form :around ((handler upload-shop-handler) action image)
  (handler-case
      (with-http-response ()
	(with-http-body ()
	  (call-next-method)))
    
    (error (e)
      (with-http-response ()
	(with-http-body ()
	  (html (:html
		 (:head
		  (:title "Error during product edit"))
		 (:body
		  (:h2 "Error during product edit")
		  (:p "Error during product edit:")
		  (:p (:princ-safe (apply #'format nil (simple-condition-format-control e) (simple-condition-format-arguments e))))
		  (:p ((:a :href "javascript:window.close()") "ok"))))))))))

(defmethod handle-object-form ((handler upload-shop-handler) (action (eql :edit)) image)
  (with-query-params (price-select)
    (let ((product (quickhoney-image-pdf-product image))
	  (price (parse-integer price-select)))
      (unless product
	(error "Could not find product for image ~A" (store-image-name image)))
      (with-transaction (:set-product-price)
	(setf (quickhoney-product-price product) price))
      (let* ((width (store-image-width image))
	     (height (store-image-height image))
	     (ratio (/ 1 (max (/ width 300) (/ height 200)))))
	
	(html (:html
	       (:head
		(:title "Price change successfully")
		((:script :type "text/javascript" :language "JavaScript")
		 "window.opener.current_image.shop_price = " (:princ-safe price) ";"
		 "window.opener.after_image_edit();")
		(:body
		 (:p "Price for image " (:princ-safe (store-image-name image)) " set to "
		     (:princ-safe price))
		 (:p ((:img :src (format nil "/image/~D" (store-object-id image))
			    :width (round (* ratio width)) :height (round (* ratio height)))))
		 (:p ((:a :href "javascript:window.close()") "ok"))))))))))
      
(defmethod handle-object-form ((handler upload-shop-handler) (action (eql :delete)) image)
  (let ((product (quickhoney-image-pdf-product image)))
    (unless product
      (error "Could not find product for image ~A" (store-image-name image)))
    (delete-object product))

  (let* ((width (store-image-width image))
	 (height (store-image-height image))
	 (ratio (/ 1 (max (/ width 300) (/ height 200)))))
    
    (html (:html
	   (:head
	    (:title "Product deleted successfully")
	    ((:script :type "text/javascript" :language "JavaScript")
	     "delete window.opener.current_image.shop_price;"
	     "delete window.opener.current_image.shop_file;"
	     "window.opener.after_image_edit();")
	    (:body
	     (:p "Product for image " (:princ-safe (store-image-name image)) " deleted")
	     (:p ((:img :src (format nil "/image/~D" (store-object-id image))
			:width (round (* ratio width)) :height (round (* ratio height)))))
	     (:p ((:a :href "javascript:window.close()") "ok"))))))))
  
(defmethod handle-object-form ((handler upload-shop-handler) (action (eql :upload)) image)
  (with-query-params (price-select pdf-generate)
    (let ((pdf-file (request-uploaded-file "pdf-image-file"))
	  (price (parse-integer price-select)))
      (when pdf-generate
	(error "PDF generation not yet supported")
	;; XXX generate PDF
	)

      (when pdf-file
	(let ((product (make-blob-from-file (upload-pathname pdf-file) 'quickhoney-pdf-product
					    :price (parse-integer price-select :junk-allowed t)
					    :type :pdf
					    :image image)))
	  
	  (let* ((width (store-image-width image))
		 (height (store-image-height image))
		 (ratio (/ 1 (max (/ width 300) (/ height 200)))))
	    (html (:html
		   (:head
		    (:title "Product uploaded successfully")
		    ((:script :type "text/javascript" :language "JavaScript")
		     "window.opener.current_image.shop_price = " (:princ-safe price) ";"
		     "window.opener.current_image.shop_file = " (:princ-safe (store-object-id product)) ";"
		     "window.opener.after_image_edit();")
		    (:body
		     (:p "Product for image " (:princ-safe (store-image-name image)) " uploaded successfully")
		     (:p ((:img :src (format nil "/image/~D" (store-object-id image))
				:width (round (* ratio width)) :height (round (* ratio height)))))
		     (:p ((:a :href "javascript:window.close()") "ok"))))))))))))

(defclass quickhoney-admin-pdf-handler (admin-only-handler object-handler)
  ()
  (:default-initargs :object-class 'quickhoney-pdf-product))

(defclass quickhoney-client-pdf-handler (prefix-handler)
  ())
  