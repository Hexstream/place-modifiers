(cl:defpackage #:place-modifiers
  (:nicknames #:place-modifier)
  (:use #:cl)
  (:import-from #:map-bind #:map-bind)
  (:import-from #:cartesian-product-switch #:cartesian-product-switch)
  (:export #:modify ; import this single symbol for normal usage.

           #:*spot-index-format*
           #:info
           #:standard-info
           #:name
           #:names
           #:inconceivable-place-p
           #:convert-spot-indexes
           #:spot-indexes
           #:spot-index
           #:parse-operator
           #:locate
           #:ensure
           #:define))
