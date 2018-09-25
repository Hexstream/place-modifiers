(cl:defpackage #:place-modifiers_tests
  (:use #:cl #:parachute)
  (:import-from #:place-modifiers #:modify))

(cl:in-package #:place-modifiers_tests)

(define-test "featured-examples"
  ;;; 3 trivial examples
  (is = 8
      (let ((place 7))
        (modify (1+ place))
        place))
  (is equal '(new old)
      (let ((place '(old)))
        (modify (cons 'new place))
        place))
  (is equal '("hello" "hi")
      (let ((place '("hello" "hi")))
        (modify (adjoin "HELLO" place :test #'string-equal))
        place))

  ;;; Equivalent to hundreds of modify macros!
  (is equal '(3 2 1)
      (let ((place (list 1 2 3)))
        (modify (nreverse place))
        place))
  (is string= "YAY"
      (let ((place "Yay"))
        (modify (string-upcase place))
        place))
  (is equal '(atom)
      (let ((place 'atom))
        (modify (list place))
        place))
  (is eq (find-class 'symbol)
      (let ((place 'symbol))
        (modify (class-of place))
        place))
  (is = 1986
      (let ((place "1986"))
        (modify (parse-integer place))
        place))

  ;;; MODIFY return values, :old
  (is equal '(256 256)
      (let ((place 2))
        (list (modify (expt place 8))
              place)))
  (is equal '(2 256)
      (let ((place 2))
        (list (modify (:old (expt place 8)))
              place)))

  ;;; PME VS place
  (is equalp '(e #((e)))
      (let ((object (vector 'e)))
        (list (modify (:old (list (aref object 0))))
              object)))
  (is equal '(((d . 4))
        (first a 1 B 2 (c . 3) (d . 4)))
      (let ((list '((d . 4))))
        (list (modify (:old (cons 'first (list* 'a 1 'b 2 (acons 'c 3 list)))))
              list)))
  (is equalp '(#(e) (e))
      (let ((object (vector 'e)))
        (list (modify (:old (list (aref (:place object) 0))))
              object)))
  (is equalp '(#(e) e)
      (let ((object (vector 'e)))
        (list (modify (:old (aref (:place object) 0)))
              object)))

  ;; Multiple PMEs: setf-like
  (is equal '((a) (b a) b)
      (let ((x 'a) (y 'b))
        (list (modify (list x)
                      (:old (cons y x)))
              x
              y))))
