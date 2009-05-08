(in-package :paypal-test)
(defgeneric dispatch-request (request-type request)
  (:documentation "dispatch incoming http request"))

(defmethod no-applicable-method ((function (eql #'dispatch-request)) &rest args)
  (declare (ignore args))
  nil)

(defmacro define-handler (type (request) &body body)
  (let ((request-type-var (gensym)))
    `(defmethod dispatch-request ((,request-type-var (eql ,type)) ,request)
       (declare (ignore ,request-type-var))
       (lambda () ,@body))))

(defvar *response-host* nil)
(defvar *response-port* nil)

(define-handler :checkout (request)
  (tbnl:redirect (paypal:make-express-checkout-url 10 :eur
                                                   (format nil "http://~A:~A/return-paypal" *response-host* *response-port*)
                                                   (format nil "http://~A:~A/cancel-paypal" *response-host* *response-port*))))

(define-handler :stop (request)
  (throw 'stop-server nil))

(define-handler :return-paypal (request)
  (with-output-to-string (*standard-output*)
    (let* ((token (tbnl:get-parameter "token"))
           (response (paypal:request "GetExpressCheckoutDetails" :token token))
           (payerid (getf response :payerid))
           (amt (getf response :amt))
           (currencycode (getf response :currencycode)))
      (print (paypal:request "DoExpressCheckoutPayment"
                             :token token
                             :payerid payerid
                             :amt amt
                             :currencycode currencycode
                             :paymentaction "Sale")))))

(define-handler :cancel-paypal (request)
  "Cancelled")

(defun dispatch-request% (request)
  (let* ((type-string (cl-ppcre:scan-to-strings "[^/]+" (tbnl:script-name request)))
         (request-type (and type-string (find-symbol (string-upcase type-string) :keyword))))
    (dispatch-request request-type request)))

(defun test-express-checkout (&key (response-port 2993) (response-host "127.0.0.1"))
  (setf *response-host* response-host
        *response-port* response-port)
  (catch 'stop-server
    (tbnl:start-server :port response-port
                       :dispatch-table (list #'dispatch-request%))))
