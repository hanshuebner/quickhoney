;; -*- Lisp -*-

(defpackage :twitter-oauth
  (:use :cl)
  (:import-from :xhtml-generator #:html)
  (:export #:callback-handler
           #:register-twitter-account-handler))

(in-package :twitter-oauth)

#+(or)
(defparameter *callback-uri* "http://quickhoney.com/twitter-oauth-callback")
#-(or)
(defparameter *callback-uri* "http://localhost:8083/twitter-oauth-callback")
(defparameter *get-request-token-endpoint* "https://api.twitter.com/oauth/request_token")
(defparameter *auth-request-token-endpoint* "https://api.twitter.com/oauth/authorize")
(defparameter *get-access-token-endpoint* "https://api.twitter.com/oauth/access_token")

(defun get-consumer-token ()
  (block nil
    (let ((config-file (asdf:system-relative-pathname :quickhoney "twitter-consumer-params.txt")))
      (with-open-file (config config-file :if-does-not-exist nil)
        (unless config
          (warn "twitter consumer params file ~A not found - cannot tweet" config-file)
          (return))
        (let* ((consumer-key (read config nil))
               (consumer-secret (read config nil)))
          (unless (and consumer-key
                       consumer-secret)
            (warn "twitter oath parameters CONSUMER-KEY and CONSUMER-SECRET not set - cannot tweet"))
          (oauth:make-consumer-token :key consumer-key :secret consumer-secret))))))

(defclass register-twitter-account-handler (bknr.web:admin-only-handler bknr.web:page-handler)
  ())

(defmethod bknr.web:handle ((handler register-twitter-account-handler))
  (bknr.web:with-query-params (action)
    (when (equal action "register")
      (let* ((account-name (hunchentoot:get-parameter "account-name"))
             (consumer-token (get-consumer-token))
             (request-token (oauth:obtain-request-token *get-request-token-endpoint*
                                                        consumer-token
                                                        :callback-uri *callback-uri*)))
        (setf (hunchentoot:session-value 'register-twitter-account-params)
              (list account-name request-token consumer-token))
        (hunchentoot:redirect (puri:render-uri (oauth:make-authorization-uri *auth-request-token-endpoint* request-token) nil))))
    (bknr.web:with-bknr-page ()
      (:form
       (cl-ppcre:register-groups-bind (account-name)
           ("^delete-(.*)" action)
         (alexandria:when-let (account (tweet:twitter-account-with-name account-name))
           (bknr.datastore:delete-object account)
           (html
            (:h2 "Twitter account " (:princ account-name) " has been deleted"))))
       (cond
         ((null (tweet:all-twitter-accounts))
          (html
           (:h2 "No Twitter accounts registered")))
         (t
          (html
           (:h2 "Registered Twitter accounts")
           ((:ul :class "twitter-account-list")
            (dolist (account (tweet:all-twitter-accounts))
              (html (:li
                     (:span (:princ (tweet:twitter-name account)))
                     ((:button :name "action" :value (format nil "delete-~A" (tweet:twitter-name account)))
                      "delete"))))))))
       (:h2 "Register new Twitter account:")
       (:label
        "Account name: " ((:input :name "account-name")))
       ((:button :type "submit" :name "action" :value "register") "register")))))

(defclass callback-handler (bknr.web:page-handler)
  ())

(defmethod bknr.web:handle ((handler callback-handler))
  (destructuring-bind (twitter-account-name request-token consumer-token)
      (hunchentoot:session-value 'register-twitter-account-params)
    (hunchentoot:delete-session-value 'register-twitter-account-params)
    (handler-case
        (oauth:authorize-request-token-from-request (lambda (request-token-key)
                                                      (unless (equal (hunchentoot:url-encode request-token-key)
                                                                     (oauth:token-key request-token))
                                                        (error "Keys differ: ~S - ~S~%"
                                                               (hunchentoot:url-encode request-token-key)
                                                               (oauth:token-key request-token)))
                                                      request-token))
      (error (c)
        (error "Couldn't verify request token authorization: ~A" c)))
    (unless (oauth:request-token-authorized-p request-token)
      (error "Request token not authorized"))
    (let ((access-token (oauth:obtain-access-token *get-access-token-endpoint* request-token)))
      (tweet:make-twitter-account twitter-account-name
                                  (oauth:token-key consumer-token)
                                  (oauth:token-secret consumer-token)
                                  (oauth:token-key access-token)
                                  (oauth:token-secret access-token)
                                  :overwritep t)
      (bknr.web:with-bknr-page ()
        (:h2 "Twitter account registered.")
        "The twitter account " (:princ twitter-account-name) " has been successfully registered"))))

