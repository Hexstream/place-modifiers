(cl:defpackage #:place-modifiers
  (:nicknames #:place-modifier)
  (:use #:cl)
  (:import-from #:map-bind #:map-bind)
  (:export #:modify ; import this single symbol for normal usage.

           #:info
           #:name
           #:place-index
           #:locate
           #:ensure
           #:define))
