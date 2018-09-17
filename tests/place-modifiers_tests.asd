(asdf:defsystem #:place-modifiers_tests

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "PLACE-MODIFIERS unit tests."

  :depends-on ("place-modifiers"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:place-modifiers_tests)))
