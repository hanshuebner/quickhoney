(in-package :paypal)

(defparameter *paypal-url* "https://api-3t.sandbox.paypal.com/nvp"
  "NVP URL of the Paypal server")
(defparameter *paypal-user* "hans.huebner_api1.gmail.com"
  "Username to use to authenticate at the Paypal server")
(defparameter *paypal-password* "62QFQPLEMM6P3M25"
  "Password to use to authenticate at the Paypal server")
(defparameter *paypal-signature* "AFcWxV21C7fd0v3bYYYRCpSSRl31Ac-RAs1SuG20a1IoPMJ0WKbx0fdG"
  "Signature to use to authenticate at the Paypal server")

(define-condition paypal-error (error)
  ())

(define-condition request-error (paypal-error)
  ((response :initarg :response)))

(define-condition http-request-error (paypal-error)
  ((http-status :initarg :http-status)
   (response-string :initarg :response-string)))

(define-condition response-error (paypal-error)
  ((response :initarg :response)
   (invalid-parameter :initarg :invalid-parameter)))

(defun decode-response (response)
  "Decode a paypal response string, which is URL encoded and follow
  list encoding rules.  Returns the parameters as a plist."
  (let ((hash (make-hash-table)))
    (dolist (entry (cl-ppcre:split "&" response))
      (destructuring-bind (parameter-string value) (cl-ppcre:split "=" entry :limit 2)
        (multiple-value-bind (match registers) (cl-ppcre:scan-to-strings "^L_(.*?)([0-9]+)$" parameter-string)
          (if match
              (let* ((parameter (intern (aref registers 0) :keyword))
                     (index (parse-integer (aref registers 1)))
                     (previous-value (gethash parameter hash)))
                (unless (= (length previous-value) index)
                  (error 'response-error :invalid-parameter parameter-string :response response))
                (setf (gethash parameter hash) (append previous-value (list (hunchentoot:url-decode value :utf-8)))))
              (setf (gethash (intern parameter-string :keyword) hash) (hunchentoot:url-decode value :utf-8))))))
    (loop for key being the hash-keys of hash
         collect key
         collect (gethash key hash))))

(defun request (method &rest args &key &allow-other-keys)
  "Perform a request to the Paypal NVP API.  METHOD is the method to
  use, additional keyword arguments are passed as parameters to the
  API.  Returns "
  (multiple-value-bind (response-string http-status)
      (drakma:http-request *paypal-url*
                           :method :post
                           :parameters (append (list (cons "METHOD" method)
                                                     (cons "VERSION" "52.0")
                                                     (cons "USER" *paypal-user*)
                                                     (cons "PWD" *paypal-password*)
                                                     (cons "SIGNATURE" *paypal-signature*))
                                               (loop for (param value) on args by #'cddr
                                                  collect (cons (symbol-name param)
                                                                (if (stringp value)
                                                                    value
                                                                    (princ-to-string value))))))
    (unless (= 200 http-status)
      (error 'http-request-error :http-status http-status :response-string response-string))
    (let ((response (decode-response response-string)))
      (unless (string-equal "Success" (getf response :ack))
        (error 'request-error :response response))
      response)))

(defun make-express-checkout-url (amount currencycode returnurl cancelurl)
  (let* ((amt (format nil "~,2F" amount))
         (currencycode (symbol-name currencycode))
         (token (getf (request "SetExpressCheckout"
                               :amt amt
                               :currencycode currencycode
                               :returnurl returnurl
                               :cancelurl cancelurl
                               :paymentaction "Sale")
                      :token)))
    (format nil "https://www.sandbox.paypal.com/webscr?cmd=_express-checkout&token=~A~
                                    &AMT=~A&CURRENCYCODE=~A&RETURNURL=~A&CANCELURL=~A"
            (hunchentoot:url-encode token)
            amt currencycode
            (hunchentoot:url-encode returnurl)
            (hunchentoot:url-encode cancelurl))))

