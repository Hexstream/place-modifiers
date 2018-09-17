(cl:defpackage #:place-modifiers_tests
  (:use #:cl #:parachute)
  (:import-from #:place-modifiers #:modify))

(cl:in-package #:place-modifiers_tests)

(define-test "featured-examples"
  ;;; 3 trivial examples
  (is =
      (let ((place 7))
        (modify (1+ place))
        place)
      8)
  (is equal
      (let ((place '(old)))
        (modify (cons 'new place))
        place)
      '(new old))
  (is equal
      (let ((place '("hello" "hi")))
        (modify (adjoin "HELLO" place :test #'string-equal))
        place)
      '("hello" "hi"))

  ;;; Equivalent to hundreds of modify macros!
  (is equal
      (let ((place (list 1 2 3)))
        (modify (nreverse place))
        place)
      '(3 2 1))
  (is string=
      (let ((place "Yay"))
        (modify (string-upcase place))
        place)
      "YAY")
  (is equal
      (let ((place 'atom))
        (modify (list place))
        place)
      '(atom))
  (is eq
      (let ((place 'symbol))
        (modify (class-of place))
        place)
      (find-class 'symbol))
  (is =
      (let ((place "1986"))
        (modify (parse-integer place))
        place)
      1986)

  ;;; MODIFY return values, :old
  (is equal
      (let ((place 2))
        (list (modify (expt place 8))
              place))
      '(256 256))
  (is equal
      (let ((place 2))
        (list (modify (:old (expt place 8)))
              place))
      '(2 256))

  ;;; PME VS place
  (is equalp
      (let ((object (vector 'e)))
        (list (modify (:old (list (aref object 0))))
              object))
      '(e #((e))))
  (is equal
      (let ((list '((d . 4))))
        (list (modify (:old (cons 'first (list* 'a 1 'b 2 (acons 'c 3 list)))))
              list))
      '(((d . 4))
        (first a 1 B 2 (c . 3) (d . 4))))
  (is equalp
      (let ((object (vector 'e)))
        (list (modify (:old (list (aref (:place object) 0))))
              object))
      '(#(e) (e)))
  (is equalp
      (let ((object (vector 'e)))
        (list (modify (:old (aref (:place object) 0)))
              object))
      '(#(e) e))

  ;; Multiple PMEs: setf-like
  (is equal
      (let ((x 'a) (y 'b))
        (list (modify (list x)
                      (:old (cons y x)))
              x
              y))
      '((a) (b a) b)))
