(in-package :quickhoney)

(defmethod rss-item-pub-date ((item quickhoney-image))
  (blob-timestamp item))

(defmethod quickhoney-image-explicit ((image quickhoney-image))
  (member :explicit (store-image-keywords image)))

(defmethod rss-item-encoded-content ((image quickhoney-image))
  (let ((is-vector (eq (quickhoney-image-category image) :vector)))
    (with-output-to-string (s)
      (html-stream
       s
       ((:div :class (format nil "newsentry news_~(~A~)" (quickhoney-image-category image)))
        ((:img :src (format nil "http://~A/image/~A"
                            (website-host)
                            (store-object-id image)))
        (:div
         (:h1 (:princ (store-image-name image)))
         (:princ (format nil "~A by ~A | "
                         (format-date-time (blob-timestamp image))
                         (if is-vector "Peter" "Nana")))
         ((:a :href (make-image-link image)) "permalink")))))
      (when (quickhoney-image-client image)
        (html-stream s :br "Client: " (:princ (quickhoney-image-client image)))))))

(defmethod rss-item-channel ((item quickhoney-image))
  "quickhoney")

(defmethod rss-item-title ((image quickhoney-image))
  (store-image-name image))

(defmethod rss-item-description ((image quickhoney-image))
  (format nil "~A~@[ (Client: ~A)~]" (store-image-name image) (quickhoney-image-client image)))

(defmethod rss-item-link ((image quickhoney-image))
  (make-image-link image))

(defmethod rss-item-guid ((image quickhoney-image))
  (make-image-link image))

(define-persistent-class quickhoney-news-item (quickhoney-image)
  ((title :update)
   (text :update)))

(defmethod quickhoney-image-spider-keywords ((item quickhoney-news-item))
  (quickhoney-news-item-title item))

(defmethod rss-item-title ((item quickhoney-news-item))
  (quickhoney-news-item-title item))

(defmethod rss-item-encoded-content ((item quickhoney-news-item))
  (concatenate 'string
               (call-next-method)
               (quickhoney-news-item-text item)))

(defclass quickhoney-rss-channel (rss-channel)
  ()
  (:metaclass persistent-class))

(defmethod rss-channel-items ((channel quickhoney-rss-channel) &key)
  (remove-if (lambda (item)
               (and (typep item 'quickhoney-image)
                    (quickhoney-image-explicit item)))
             (call-next-method)))

