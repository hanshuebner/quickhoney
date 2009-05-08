(in-package :quickhoney)

(define-persistent-class quickhoney-product ()
  ((image
    :update
    :type (or quickhoney-image null)
    :index-type hash-index
    :index-reader products-for-image
    :documentation
    "Image that this product is related to, or nil for standalone products"))
  (:documentation
   "Mixin class for Quickhoney products, which are usually related to a QUICKHONEY-IMAGE"))