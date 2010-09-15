(in-package :quickhoney)

(defparameter *paypal-sandbox-api-url* "https://api-3t.sandbox.paypal.com/nvp")

(defun paypal-init (&key user password signature)
  (cl-paypal:init
   *paypal-sandbox-api-url*
   user password signature
   (format nil "~A/paypal-return" quickhoney.config:*website-url*)
   (format nil "~A/paypal-cancel" quickhoney.config:*website-url*)
   :useraction "commit"
   :currencycode "USD"))

