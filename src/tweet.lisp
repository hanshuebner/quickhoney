(defpackage :tweet
  (:use :cl)
  (:export #:post-image
           #:twitter-name
           #:twitter-account-with-name
           #:all-twitter-accounts
           #:twitter-account
           #:make-twitter-account))

(in-package :tweet)

(defclass twitter-account (bknr.datastore:store-object)
  ((twitter-name :initarg :twitter-name
                 :reader twitter-name
                 :index-type bknr.indices:string-unique-index
                 :index-reader twitter-account-with-name
                 :index-values all-twitter-accounts)
   (consumer-token-key :initarg :consumer-token-key
                       :reader consumer-token-key)
   (consumer-token-secret :initarg :consumer-token-secret
                          :reader consumer-token-secret)
   (access-token-key :initarg :access-token-key
                     :reader access-token-key)
   (access-token-secret :initarg :access-token-secret
                        :reader access-token-secret))
  (:metaclass bknr.datastore:persistent-class))

(defmethod print-object ((twitter-account twitter-account) stream)
  (bknr.datastore:print-store-object (twitter-account stream :type t)
    (format stream "NAME: ~S" (twitter-name twitter-account))))

(defun make-twitter-account (twitter-name
                             consumer-token-key
                             consumer-token-secret
                             access-token-key
                             access-token-secret
                             &key overwritep)
  (bknr.datastore:with-transaction ()
    (when (and overwritep
               (twitter-account-with-name twitter-name))
      (bknr.datastore:delete-object (twitter-account-with-name twitter-name)))
    (make-instance 'twitter-account
                   :twitter-name twitter-name
                   :consumer-token-key consumer-token-key
                   :consumer-token-secret consumer-token-secret
                   :access-token-key access-token-key
                   :access-token-secret access-token-secret)))

(defparameter *twitter-url* "https://api.twitter.com/1.1/statuses/update_with_media.json")

(defun store-image-to-temporary-file (store-image)
  (bknr.images:with-store-image* (store-image)
    (let* ((file-type (if (eql (bknr.datastore:blob-type store-image) :gif)
                          :png
                          (bknr.datastore:blob-type store-image)))
           (pathname (temporary-file:with-open-temporary-file (f :keep t :template (format nil "temporary-files:temp-%.~(~A~)" file-type))
                       (pathname f))))
      (cl-gd:write-image-to-file pathname :type file-type :if-exists :supersede)
      pathname)))

(defun post-image (store-image text twitter-account-name)
  (let* ((twitter-account (twitter-account-with-name twitter-account-name))
         (access-token (make-instance 'cl-oauth:access-token
                                      :consumer (make-instance 'cl-oauth:consumer-token
                                                               :key (consumer-token-key twitter-account)
                                                               :secret (consumer-token-secret twitter-account))
                                      :key (access-token-key twitter-account)
                                      :secret (access-token-secret twitter-account)))
         (temporary-image-pathname (store-image-to-temporary-file store-image)))
    (unwind-protect
         (multiple-value-bind (response status)
             (cl-oauth:access-protected-resource *twitter-url*
                                                 access-token
                                                 :request-method :post
                                                 :include-user-parameters-in-signature-p nil
                                                 :user-parameters `(("media[]" . ,temporary-image-pathname)
                                                                    ("status" . ,text)))
           (unless (stringp response)
             (setf response (flexi-streams:octets-to-string response)))
           (if (= 200 status)
               response
               (error "can't update twitter status: ~A ~A" status response)))
      (ignore-errors (delete-file temporary-image-pathname)))))
