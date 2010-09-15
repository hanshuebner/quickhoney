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
    :documentation
    "Image that this product is related to, or nil for standalone products"))
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