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
    :relaxed-object-reference t
    :documentation
    "Image that this product is related to, or nil for standalone products")
   (active
    :update
    :initform nil
    :documentation
    "Set this to T to have real clients see the product as well"))
  (:documentation
   "Mixin class for Quickhoney products, which are usually related to a QUICKHONEY-IMAGE"))

(define-persistent-class quickhoney-pdf-product (blob quickhoney-product)
  ()
  (:documentation "Download PDF Quickhoney product. The blob data is the PDF file"))

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
  (with-query-params (price-select pdf-activate)
    (let ((product (quickhoney-image-pdf-product image))
	  (price (parse-integer price-select)))
      (unless product
	(error "Could not find product for image ~A" (store-image-name image)))
      (with-transaction (:set-product-price)
	(setf (quickhoney-product-price product) price
	      (quickhoney-product-active product )
	      (and pdf-activate
		   (string-equal pdf-activate "on"))))
      (setf *last-image-upload-timestamp* (get-universal-time))      
      (let* ((width (store-image-width image))
	     (height (store-image-height image))
	     (ratio (/ 1 (max (/ width 300) (/ height 200)))))
	
	(html (:html
	       (:head
		(:title "Price changed successfully")
		((:script :type "text/javascript" :language "JavaScript")
		 "window.opener.current_image.shop_price = " (:princ-safe price) ";"
		 "window.opener.current_image.shop_active = " (:princ-safe (quickhoney-product-active product)) ";"
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
    (delete-object product)
    (setf *last-image-upload-timestamp* (get-universal-time)))
    
  

  (let* ((width (store-image-width image))
	 (height (store-image-height image))
	 (ratio (/ 1 (max (/ width 300) (/ height 200)))))
    
    (html (:html
	   (:head
	    (:title "Product deleted successfully")
	    ((:script :type "text/javascript" :language "JavaScript")
	     "delete window.opener.current_image.shop_price;"
	     "delete window.opener.current_image.shop_file;"
	     "delete window.opener.current_image.shop_active;"
	     "window.opener.after_image_edit();")
	    (:body
	     (:p "Product for image " (:princ-safe (store-image-name image)) " deleted")
	     (:p ((:img :src (format nil "/image/~D" (store-object-id image))
			:width (round (* ratio width)) :height (round (* ratio height)))))
	     (:p ((:a :href "javascript:window.close()") "ok"))))))))

(defmethod generate-pixel-pdf ((image quickhoney-image) price &optional active)
  (with-temporary-file (s :defaults #p"/tmp/")
    (format t "temporary file: ~A~%" s)
    (pixel-pdf::convert-store-image-to-pdf image s)
    (let ((product (make-blob-from-file s 'quickhoney-pdf-product
					:price price
					:active active
					:type :pdf
					:image image)))
      (setf *last-image-upload-timestamp* (get-universal-time))      
      (format t "Convert image ~A to PDF product ~A, active ~A~%" image product active)

      )))
  
(defmethod handle-object-form ((handler upload-shop-handler) (action (eql :upload)) image)
  (with-query-params (price-select pdf-generate pdf-activate)
    (let ((pdf-file (request-uploaded-file "pdf-image-file"))
	  (price (parse-integer price-select)))
      (format t "activate: ~A~%" pdf-activate)
      (cond
	(pdf-generate
	 (bt:make-thread #'(lambda () (generate-pixel-pdf image price (and pdf-activate
									   (string-equal pdf-activate "on"))))
			 :name (format nil "GENERATE-PDF ~A" (store-image-name image)))
	 (let* ((width (store-image-width image))
		(height (store-image-height image))
		(ratio (/ 1 (max (/ width 300) (/ height 200)))))
	   (html (:html
		  (:head
		   (:title "Generating PDF for image...")
		   ((:script :src "/static/js/MochiKit/MochiKit.js,js/helpers.js" :type "text/javascript"))
		   ((:script :src "/static/js/javascript.js,js/pdf_shop.js" :type "text/javascript")))
		  (:body
		   ((:p :id "information")
		    "Generating PDF for image..."
		       ((:span :id "cue")
			((:img :src "/static/images/spinner.gif" :width 16 :height 16)))
		    )
		   (:p ((:img :src (format nil "/image/~D" (store-object-id image))
			      :width (round (* ratio width)) :height (round (* ratio height))))
		       )
		   ((:p :id "ok") ((:a :href "javascript:window.close()") "ok"))
		   ((:p :id "footer") "")
		   ((:script :type "text/javascript" :language "JavaScript")
		    "wait_for_pdf_generation_upload();")
		   )))))

	(pdf-file
	 (let ((product (make-blob-from-file (upload-pathname pdf-file) 'quickhoney-pdf-product
					     :price price
					     :active (and pdf-activate
							  (string-equal pdf-activate "on"))
					     :type :pdf
					     :image image)))
	   (setf *last-image-upload-timestamp* (get-universal-time))      
	   
	   (let* ((width (store-image-width image))
		  (height (store-image-height image))
		  (ratio (/ 1 (max (/ width 300) (/ height 200)))))
	     (html (:html
		    (:head
		     (:title "Product uploaded successfully")
		     ((:script :type "text/javascript" :language "JavaScript")
		      "window.opener.current_image.shop_price = " (:princ-safe price) ";"
		      "window.opener.current_image.shop_active = " (:princ-safe
								    (if (quickhoney-product-active product)
									"true"
									"false")) ";"
		      "window.opener.current_image.shop_file = " (:princ-safe (store-object-id product)) ";"
		      "window.opener.after_image_edit();")
		     (:body
		      (:p "Product for image " (:princ-safe (store-image-name image)) " uploaded successfully")
		      (:p ((:img :src (format nil "/image/~D" (store-object-id image))
				 :width (round (* ratio width)) :height (round (* ratio height)))))
		      (:p ((:a :href "javascript:window.close()") "ok")))))))))))))

;;; PDF Handlers

(defclass quickhoney-admin-pdf-handler (admin-only-handler object-handler)
  ()
  (:default-initargs :object-class 'quickhoney-pdf-product))

(defmethod handle-object ((handler quickhoney-admin-pdf-handler) product)
  (handle-if-modified-since (blob-timestamp product))
  (setf (header-out :last-modified) (rfc-1123-date (blob-timestamp product)))
  (with-http-response (:content-type "application/pdf")
    (setf (header-out :content-length) (blob-size product))
    (with-open-file (blob-data (blob-pathname product) :element-type '(unsigned-byte 8))
      (copy-stream blob-data (send-headers) :element-type '(unsigned-byte 8)))))
  

