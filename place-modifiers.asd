(asdf:defsystem #:place-modifiers

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Essentially gives access to hundreds of modify macros with one single symbol: MODIFY."

  :depends-on (#:map-bind
               #:cartesian-product-switch)

  :version "2.0.1"
  :serial cl:t
  :components ((:file "package")
               (:file "info")
               (:file "main")
               (:file "definitions")))
