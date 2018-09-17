(asdf:defsystem #:place-modifiers

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Essentially gives access to hundreds of modify-macros through one single macro: MODIFY."

  :depends-on (#:map-bind)

  :version "3.0"
  :serial cl:t
  :components ((:file "package")
               (:file "info")
               (:file "main")
               (:file "definitions"))

  :in-order-to ((asdf:test-op (asdf:test-op #:place-modifiers_tests))))
