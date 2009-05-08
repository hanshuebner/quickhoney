(in-package :twitter)

(define-persistent-class account ()
  ((user :read
         :type bknr.user:user
         :documentation "USER that this Twitter account belongs to")
   (authorization :update
                  :documentation "List of username and password for this account")))

(define-condition cannot-update-status (error)
  ((result :initarg :result :reader result)))

(define-condition no-account-for-user (error)
  ((user :initarg :user :reader user)))

(defgeneric update-status (who status-string &key)

  (:method ((account account) status-string &key)
    (let ((result (babel:octets-to-string
                   (drakma:http-request "http://twitter.com/statuses/update.xml"
                                        :method :post
                                                :content (format nil "status=~A&source=quickhoney"
                                                                 (hunchentoot:url-encode status-string))
                                                :content-type "application/x-www-form-urlencoded"
                                                :basic-authorization (account-authorization account)))))
      (when (cl-ppcre:scan "<error>" result)
        (error 'cannot-update-status :result result))))

  (:method ((user bknr.user:user) status-string &key errorp)
    (let ((account (find user (class-instances 'account) :key #'account-user)))
      (if account
          (update-status account status-string)
          (when errorp
            (error 'no-account-for-user :user user))))))