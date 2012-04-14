(asdf:defsystem #:place-modifiers

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Gives access to many place-modifiers (essentially modify-macros) with a single symbol: MODIFY."
  
  :version "0.1"
  :serial cl:t
  :components ((:file "package")
	       (:file "main")))
