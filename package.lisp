(cl:defpackage #:place-modifiers
  (:nicknames #:place-modifier)
  (:use #:cl)
  (:import-from #:map-bind #:map-bind)
  (:export #:modify ; import this single symbol for normal usage.

           #:info
           #:standard-info
           #:name
           #:inconceivable-place-p
           #:spot-indexes
           #:spot-index
           #:parse-operator
           #:locate
           #:ensure
           #:define))
