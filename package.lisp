(cl:defpackage #:place-modifiers
  (:nicknames #:place-modifier)
  (:use #:cl)
  (:import-from #:map-bind #:map-bind)
  (:export #:modify ; import this single symbol for normal usage.

           #:*spot-index-format*
           #:map-infos
           #:info
           #:standard-info
           #:name
           #:names
           #:inconceivable-place-p
           #:convert-spot-index
           #:default-spot-index
           #:locate
           #:ensure
           #:define))
