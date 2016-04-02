(in-package :quickhoney)

(defun paypal-init (&key (user *paypal-user*)
                    (password *paypal-password*)
                    (signature *paypal-signature*))
  (cl-paypal:init
   *paypal-url*
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

;; each transaction with paypal is modelled with a
;; PAYPAL-PRODUCT-TRANSACTION object. This object is persistent, as
;; the communication with the paypal API is asynchronous (HTTP).
;;
;; each transaction is linked to the product it is for. The product
;; can be deleted later on, but the transaction still exists, the slot
;; is relaxed.
(define-persistent-class paypal-product-transaction ()
  ((product :read
            :index-type hash-index
            :relaxed-object-reference t
            :index-reader paypal-transactions-for-product)
   (token :update
          :index-type string-unique-index
          :index-reader paypal-transaction-for-token
          :index-values all-paypal-transactions)
   (link :update
         :documentation "Link to paypal express checkout"
         :initform nil)
   (button-link :update
         :documentation "Link to paypal express checkout button"
         :initform nil)
   (status :update :initform :ongoing
           :documentation "Can be either :REQUESTING, :ONGOING, :CANCELLED, :SUCCESSFUL, :ERROR")
   (watermarked-pdf :update :initform nil
                    :documentation "Path to watermarked PDF")
   (creation-time :update :initform (get-universal-time))
   (valid-time :update :initform (get-universal-time))
   (paypal-result :update :initform nil)
   (paypal-info :update :initform nil)))

(defmethod paypal-txn-deleted-p ((txn paypal-product-transaction))
  "Check if the product the transaction was deleted or the image of
the product was deleted."
  (let ((product (paypal-product-transaction-product txn)))
    (or (null product)
        (object-destroyed-p product)
        (let ((image (quickhoney-product-image product)))
          (or (null image)
              (object-destroyed-p image))))))

(defmethod paypal-txn-valid-p ((txn paypal-product-transaction))
  "Check that the transaction was successful, hasn't expired and that
  the product still exists."
  (and (eql (paypal-product-transaction-status txn)
            :successful)
       (not (paypal-txn-expired-p txn))
       (not (paypal-txn-deleted-p txn))))

(defmethod paypal-txn-valid-until ((txn paypal-product-transaction))
  (+ (paypal-product-transaction-valid-time txn)
     (* *product-validity-time* 24 3600)))

(defmethod paypal-txn-expired-p ((txn paypal-product-transaction))
  (< (paypal-txn-valid-until txn)
     (get-universal-time)))

(defmethod paypal-txn-reactivate ((txn paypal-product-transaction))
  (with-transaction ()
    (setf (paypal-product-transaction-valid-time txn)
          (get-universal-time))))

(defmethod paypal-txn-client-email ((txn paypal-product-transaction))
  (getf (paypal-product-transaction-paypal-info txn)
        :email))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JSON paypal checkout handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass json-paypal-checkout-handler (page-handler)
  ()
  (:documentation "Creates an express checkout URL to paypal. This
  creates a persistent PAYPAL-PRODUCT-TRANSACTION object."))

(defun json-paypal-checkout-txn-to-json (txn)
  "Encode the transaction object as JSON to pass back to the
  javascript."
  (with-slots (link button-link status) txn
    (yason:encode-object-element "id" (store-object-id txn))
    (yason:encode-object-element "status" status)
    (when link
      (yason:encode-object-element "paypalLink" link))
    (when button-link
      (yason:encode-object-element "buttonLink" button-link))))

(defmethod handle ((handler json-paypal-checkout-handler))
  "Create a checkout link by querying the paypal API. As this is
  blocking an can take some time, a new thread is created. The
  resulting transaction is encoded to json."
  (with-json-response ()
    (with-query-params (image color id)
      (if id
          (let ((txn (find-store-object (parse-integer id) :class 'paypal-product-transaction)))
            (json-paypal-checkout-txn-to-json txn))
          (let* ((img-id (parse-integer image))
                 (img (find-store-object img-id :class 'quickhoney-image))
                 (product (when img (quickhoney-image-pdf-product img))))
            (if (and img product)
                (let* ((txn (make-instance 'paypal-product-transaction
                                           :product product
                                           :token nil
                                           :status :requesting))
                       (ip (real-remote-addr)))
                  (bt:make-thread #'(lambda () (get-express-checkout-url ip txn product color))
                                  :name (format nil "GET-EXPRESS-CHECKOUT-URL ~A" (store-image-name img)))
                  (json-paypal-checkout-txn-to-json txn))
                (progn
                  (yason:encode-object-element "status" "error")
                  (yason:encode-object-element "message" "Could not find PDF for image"))))))))

(defun get-express-checkout-url (ip txn product color)
  "Do the actual API request to paypal to get an express checkout url,
  and store the resulting information persistently in the
  PAYPAL-PRODUCT-TRANSACTION object."
  (handler-case
      (let* ((price (quickhoney-product-price product))
             (img (quickhoney-product-image product))
             (id (store-object-id img)))
        (multiple-value-bind (link res)
            (cl-paypal:make-express-checkout-url price ip
                                                 :l_paymentrequest_0_name0 (store-image-name img)
                                                 :l_paymentrequest_0_number0 id
                                                 :l_paymentrequest_0_amt0 price
                                                 :l_paymentrequest_0_qt0 1
                                                 :paymentrequest_0_itemamt price
                                                 :hdrbordercolor color
                                                 :allownote 0
                                                 :noshipping 1
                                                 :sandbox *paypal-use-sandbox*
                                                 :hdrimg (format nil "http://quickhoney.ruinwesen.com/image/66899/thumbnail"))
            
            (let ((token (getf res :token)))
              (make-instance 'paypal-action-log
                             :result res :token token)

              (with-transaction ()
                (setf (paypal-product-transaction-token txn) token
                      (paypal-product-transaction-link txn) link
                      (paypal-product-transaction-button-link txn) 
                      (format nil "~a/dynamicimageweb?cmd=_dynamic-image&pal=~A"
                              *paypal-fpdbs-url*
                              *paypal-secure-merchant-id*)
                      (paypal-product-transaction-status txn) :ongoing)))))
    (error (e)
      (format t "error while paypal ~A~%" e)
      (with-transaction ()
        (setf (paypal-product-transaction-status txn) :error)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paypal success handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass paypal-success-handler (page-handler)
  ())

(defmethod send-transaction-email ((txn paypal-product-transaction))
  (with-slots (status product token creation-time valid-time paypal-info) txn
    (let ((image (quickhoney-product-image product))
          (from *paypal-email*)
;;        (from "p@quickhoney.com")
          (to (paypal-txn-client-email txn))
;;        (to "hans@huebner.org")
;;        (
;;        to "manuel@bl0rg.net")
          (subject "Download your Vector PDF File!"))
      (cl-smtp:with-smtp-mail (smtp "localhost"
                                    from
                                    (list to))
;;                                  :port 25)
        #-nil
        (progn
          (format smtp "To: ~a~A~A" to #\Return #\Newline)
          (format smtp "From: ~a~A~A" from #\Return #\Newline)
          (format smtp "Subject: ~a~A~A" subject #\Return #\Newline))
        
        (cl-mime:print-mime
         smtp
         (make-text-html-email
          (with-output-to-string (s)
            (format s "Download your Vector PDF File!~%~%")
            (format s "Dear ~A ~A,~%Thank you for purchasing the following Vector PDF File!~%"
                    (getf paypal-info :firstname) (getf paypal-info :lastname))
            (format s "FILE#: ~A~%" (store-object-id image))
            (format s "Filetype: Vector PDF~%")
            (format s "Filesize: ~Akb~%" (floor (blob-size image) 1024))
            (format s "Price: ~A$~%"(quickhoney-product-price product))
            (format s "Download Artwork ~A for one-time private use only at the following URL:~% ~A/#paypal/~A~%~%"
                    (store-image-name image)
                    *website-url* token)
            (format s "Thank you very much for your purchase,~%QuickHoney~%"))
          (with-html-email ()
            (:html
             (:head (:title "Download your Vector PDF File!"))
             (:body
              "Dear " (:princ-safe (getf paypal-info :firstname)) " " (:princ-safe (getf paypal-info :lastname)) ", "
              (:br)(:br)
              "Thank you for purchasing the following Vector PDF File!"
              (:br) (:br)
              ((:img :src (format nil "~A/image/~A/thumbnail,,160,160" *website-url* (store-object-id image))))
              (:br)(:br)
              "FILE#: " (:princ-safe (store-object-id image)) (:br)
              "Filetype: Vector PDF" (:br)
              "Filesize: " (:princ-safe (floor (blob-size image) 1024)) "kb" (:br)
              "Price: " (:princ-safe (quickhoney-product-price product)) "$" (:br)(:br)
              "Download Artwork " ((:a :href (format nil "~A/#paypal/~A" *website-url* token))
                                   (:princ-safe (store-image-name image)))
              " for private, non-commercial use only." (:br) (:br)
              "If you have trouble clicking the above link, please follow this link "
              (:princ-safe (format nil "~A/#paypal/~A" *website-url* token)) (:br) (:br)
              "Thank you very much for your purchase," (:br) (:br)
              ((:img :src (format nil "~A/image/~A" *website-url* "QH_Logo_small"))) (:br)
              "QuickHoney")
             )))
         t t)))))

(defmethod paypal-process-successful-transaction ((txn paypal-product-transaction))
  (send-transaction-email txn))

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

      (if (string-equal (getf result :ACK) "Success")
          (progn
            (with-transaction ()
              (setf (paypal-product-transaction-creation-time txn) (get-universal-time)
                    (paypal-product-transaction-paypal-result txn) result
                    (paypal-product-transaction-paypal-info txn) response
                    (paypal-product-transaction-valid-time txn) (get-universal-time)
                    (paypal-product-transaction-status txn) :successful))
            (handler-case 
                (paypal-process-successful-transaction txn)
              (error (e)
                (bknr.web::do-error-log-request e)
                )))

          (with-transaction ()
            (setf (paypal-product-transaction-creation-time txn) (get-universal-time)
                  (paypal-product-transaction-paypal-result txn) result
                  (paypal-product-transaction-paypal-info txn) response
                  (paypal-product-transaction-valid-time txn) (get-universal-time)
                  (paypal-product-transaction-status txn) :error)))
      
      (redirect (format nil "/#paypal/~A" token)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paypal cancel handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
              (paypal-product-transaction-paypal-info txn) response
              (paypal-product-transaction-status txn) :cancelled))
      
      (redirect (format nil "/#paypal/~A" token)))))

(defun transaction-with-download-id (id)
  (paypal-transaction-for-token id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PDF and information endpoints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *signature-svg-pathname*
  (merge-pathnames "files/signature.svg" *root-directory*))

(defmethod add-transaction-watermark ((tx paypal-product-transaction) pathname)
  "Add a watermark to the PDF by running a fugly shell script doing
   some pdfmerge voodoo."
  (with-slots (product paypal-info) tx
    (let* ((image (quickhoney-product-image product))
           (pdf-location (blob-pathname product))
           (firstname (getf paypal-info :firstname))
           (lastname (getf paypal-info :lastname))
           (watermark-path (merge-pathnames "bin/watermark-pdf.sh" *root-directory*)))
      (run-shell-command-to-string "\"~A\" \"~A\" \"~A\" \"~A\" \"~A ~A\""
                                   watermark-path pdf-location pathname *signature-svg-pathname* firstname lastname))))
           
(defclass quickhoney-client-pdf-handler (object-handler)
  ()
  (:default-initargs :query-function #'transaction-with-download-id
    :object-class 'paypal-product-transaction)
  (:documentation "Serves the actual PDF to the client."))

(defmethod handle-object ((handler quickhoney-client-pdf-handler) txn)
  "Serve the PDF file for the client by adding a watermark, and sending the resulting data to the HTTP stream."
  (cond ((paypal-txn-valid-p txn)
	 (let ((product (paypal-product-transaction-product txn)))
	   (handle-if-modified-since (blob-timestamp product))
	   (setf (header-out :last-modified) (rfc-1123-date (blob-timestamp product)))
	   (with-http-response (:content-type "application/pdf")
	     (with-temporary-file (tmp :defaults #p"/tmp/")
	       (add-transaction-watermark txn tmp)
	       (with-open-file (blob-data tmp :element-type '(unsigned-byte 8))
		 (setf (header-out :content-length) (file-length blob-data))
		 (copy-stream blob-data (send-headers) :element-type '(unsigned-byte 8)))))))
	(t (redirect (format nil "/#paypal/~A" (store-object-id txn))))))

(defclass json-paypal-transaction-info-handler (object-handler)
  ()
  (:default-initargs :query-function #'transaction-with-download-id
    :object-class 'paypal-product-transaction)
  (:documentation "Returns information about the paypal transaction as
  JSON, so that it can be displayed on the download page, and queried
  in loops waiting for a paypal transaction status change."))

(defmethod handle-object ((handler json-paypal-transaction-info-handler) txn)
  (with-json-response ()
    (with-slots (product token status creation-time) txn
      (yason:encode-object-element "token" token)
      (yason:encode-object-element "status" status)
      (yason:encode-object-element "valid" (paypal-txn-valid-p txn))
      (yason:encode-object-element "bought_on" (format-date-time (paypal-product-transaction-creation-time txn)
                                                                :us-style t :show-time nil))
      (yason:encode-object-element "valid_until" (format-date-time (paypal-txn-valid-until txn)
                                                                  :us-style t :show-time nil))
      (yason:encode-object-element "expired" (paypal-txn-expired-p txn))
      (yason:encode-object-element "deleted" (paypal-txn-deleted-p txn))
      (unless (paypal-txn-deleted-p txn)
        (yason:with-object-element ("image")
          (image-to-json (quickhoney-product-image product)))))))

(defclass json-paypal-admin-handler (object-handler admin-only-handler)
  ()
  (:documentation "Admin version of the JSON information handler,
  giving back more information (used for the admin frontend)."))

(defun assoc-to-json (assoc)
  (yason:with-object ()
    (loop for (key value) on assoc by #'cddr
       do (yason:encode-object-element (string-downcase (symbol-name key)) value))))

(defmethod paypal-txn-to-json ((txn paypal-product-transaction))
  (yason:with-object ()
    (with-slots (product token status creation-time valid-time paypal-result paypal-info) txn
      (yason:encode-object-element "token" token)
      (yason:encode-object-element "status" status)

      (when (not (paypal-txn-deleted-p txn))
        (yason:with-object-element ("image")
          (yason:with-object ()
            (let ((image (quickhoney-product-image product)))
              (yason:encode-object-element "name" (store-image-name image))
              (yason:encode-object-element "id" (store-object-id image))
              (yason:encode-object-element "width" (store-image-width image))
              (yason:encode-object-element "height" (store-image-height image))))))
          
      (yason:encode-object-element "bought_on" (format-date-time (paypal-product-transaction-creation-time txn)
                                                                :us-style t :show-time t))
      (yason:encode-object-element "valid_until" (format-date-time (paypal-txn-valid-until txn)
                                                                  :us-style t :show-time t))
      (yason:encode-object-element "creation_time" (paypal-product-transaction-creation-time txn))
      (yason:encode-object-element "valid_time" (paypal-product-transaction-valid-time txn))
      (yason:encode-object-element "valid" (paypal-txn-valid-p txn))
      (yason:encode-object-element "deleted" (paypal-txn-deleted-p txn))
      (yason:encode-object-element "expired" (paypal-txn-expired-p txn))
      (yason:with-object-element ("paypal_result")
        (assoc-to-json paypal-result))
      (yason:with-object-element ("paypal_info")
        (assoc-to-json paypal-info)))))

;; XXX this is like the least efficient function i've ever written
(defun find-paypal-transactions (&key token id from until count status)
  "Find paypal transaction matching certain criterias: either TOKEN, ID, a date range, or a status."
  (cond ((not (null token))
         (list (paypal-transaction-for-token token)))
        ((not (null id))
         (list (find-store-object id :class 'paypal-product-transaction)))
        (t (let ((txs (copy-list (all-paypal-transactions))))
             (unless (null status)
               (setf txs (remove-if-not #'(lambda (x) (eql x status)) txs
                                        :key #'paypal-product-transaction-status)))
             (unless (null from)
               (setf txs (remove-if-not #'(lambda (x) (> x from)) txs
                                        :key #'paypal-product-transaction-creation-time)))
             (unless (null until)
               (setf txs (remove-if-not #'(lambda (x) (< x until)) txs
                                        :key #'paypal-product-transaction-creation-time)))
             (setf txs (sort txs #'> :key #'paypal-product-transaction-creation-time))
             (if count
                 (subseq txs 0 (min count (length txs)))
                 txs)))))

(defmethod object-handler-get-object ((handler json-paypal-admin-handler))
  "Parse a JSON request to find certain paypal transaction."
  (with-query-params (token id from until count status)
    (when status
      (setf status (make-keyword-from-string status)))
    (when from
      (setf from (parse-integer from)))
    (when until
      (setf until (parse-integer until)))
    (when count
      (setf count (parse-integer count)))
    (find-paypal-transactions :id id :token token :from from :until until :count count :status status)))

(defmethod handle-object ((handler json-paypal-admin-handler) txns)
  "Backend for the JSON paypal transactions. Allows to either find
transactions matching specific criteria, or reactivate a specific
paypal transaction."
  (with-query-params (action)
    (cond ((string-equal action "reactivate")
           (with-query-params (sendemail)
             (map nil #'paypal-txn-reactivate txns)
             (when sendemail
               (map nil #'send-transaction-email txns))))
          ((null action) )
          (t (error "Unknown action ~A" action))))
  (with-json-response ()
    (yason:with-object-element ("paypalTransactions")
      (yason:with-array ()
        (dolist (txn txns)
          (paypal-txn-to-json txn))))))
