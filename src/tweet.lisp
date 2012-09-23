(defpackage :tweet
  (:use :cl)
  (:export #:post-image))

(in-package :tweet)

(defparameter *twitter-access-token*
  (make-instance 'cl-oauth:access-token
                 :consumer (make-instance 'cl-oauth:consumer-token
                                          :key "ksOwWvxkBtzwhFP3yThlhw"
                                          :secret "MFUfBQQQrDgfI0HDeQAhgCvdlp8UEsBEVTNCkSsrbdA")
                 :key "841355653-bZMR7iD6CYyS8XnzAFCWNthkwGBDogI1197aBg1M"
                 :secret "RKNoAIUWQapu3wAiZa932VifZZsff8UXQZ78DfsKOMw"))

(defparameter *twitter-url* "https://upload.twitter.com/1/statuses/update_with_media.json")

(defun post-image (text image)
  (multiple-value-bind (response status)
      (cl-oauth:access-protected-resource *twitter-url*
                                          *twitter-access-token*
                                          :request-method :post
                                          :user-parameters `(("media[]" . #P"/home/hans/bos/lib/bknr-datastore/doc/eboyshot1.png")
                                                             ("status" . ,text)))
    (unless (stringp response)
      (setf response (flexi-streams:octets-to-string response)))
    (if (= 200 status)
        response
        (error "can't update twitter status: ~A ~A" status response))))