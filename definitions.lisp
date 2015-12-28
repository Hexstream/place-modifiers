(in-package #:place-modifiers)

(place-modifier:define ()
  ((* + - /) t)
  ((1+ 1-) t)
  ((< <= = /= > >=) t)
  (abs t)
  (acons t 3)
  ((sin cos tan
        asin acos ;atan (see below)
        sinh cosh tanh
        asinh acosh atanh
        cis)
   t)
  (atan t)
  (adjoin t 2)
  (adjust-array t)
  (adjustable-array-p nil)
  (allocate-instance t)
  (alpha-char-p t)
  (alphanumericp t)
  ((and or) t)
  ((append nconc list*) t -1)
  (apply nil -1)
  (funcall t 2)
  (apropos-list t)
  (aref nil)
  ((elt svref row-major-aref) nil)
  ((arithmetic-error-operands arithmetic-error-operation) nil)
  (array-dimension nil)
  (array-dimensions nil)
  (array-displacement nil) ; Ignore second return value.
  ((array-element-type
    array-has-fill-pointer-p
    array-in-bounds-p
    array-rank)
   nil)
  (array-row-major-index nil)
  (array-total-size nil)
  (arrayp t)
  (ash t)
  ((assoc assoc-if assoc-if-not rassoc rassoc-if rassoc-if-not)
   nil 2)
  (atom t)
  ((bit sbit) nil)
  ((bit-and
    bit-andc1
    bit-andc2
    bit-eqv
    bit-ior
    bit-nand
    bit-nor
    bit-orc1
    bit-orc2
    bit-xor)
   t)
  (bit-not t)
  (bit-vector-p t)
  (boole t 2))

#+nil(define-modify-macro (modify boolean) (&place generalized-boolean) ; caution
       (lambda (generalized-boolean)
         (if generalized-boolean t nil)))

(place-modifier:define ()
  ((both-case-p lower-case-p upper-case-p) t)
  (boundp nil)
  (broadcast-stream-streams nil)
  ((butlast nbutlast) nil)
  (byte t)
  ((byte-position byte-size) nil)
  ((car cdr caar cadr cdar cddr
        caaar caadr cadar caddr cdaar cdadr cddar cdddr
        caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
        cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
   nil)
  ((first second third fourth fifth sixth seventh eighth ninth tenth)
   nil)
  ;; Todo: Leave as-is without writeback when empty body.
  #+nil((case ccase ecase typecase ctypecase etypecase)
        t)

  ((ceiling fceiling floor ffloor round fround truncate ftruncate)
   t) ; Ignore second return value.
  (cell-error-name nil)
  ((char schar) nil)
  ((char-code code-char char-int char-name char-downcase char-upcase)
   t)
  ((char-equal char-greaterp char-lessp
               char-not-equal char-not-greaterp char-not-lessp
               char/= char< char<= char= char> char>=)
   t)
  ((character characterp) t)
  (class-name t)
  ;; Let's be pragmatic, even if a crazy dialect could(?) allow (setf class-of).
  ((class-of type-of) t)
  (close t)
  (coerce t))

#+nil(define-modify-macro (modify compile) (must-be-nil &place lambda-expression) ; caution
       (lambda (must-be-nil lambda-expression)
         (when must-be-nil
           (error "The first argument to MODIFY COMPILE must be NIL."))
         (unless (typep lambda-expression '(cons (eql lambda)))
           (error "COMPILEF can only compile lambda expressions."))
         (compile nil lambda-expression)))

(place-modifier:define ()
  ((compile-file ; Ignore second and third return values.
    compile-file-pathname
    compiled-function-p)
   t)
  ((compiler-macro-function macro-function) nil) ; Omit pointless secondary variant.
  (complement t)
  (complex t)
  (complexp t)
  (compute-applicable-methods t)
  (compute-restarts t)
  (concatenate t 2)
  (concatenated-stream-streams nil)
  (conjugate t)
  (cons t 2)
  (consp t)
  (constantly t)
  (constantp t)
  ((copy-alist
    copy-list
    copy-pprint-dispatch
    copy-seq
    copy-structure
    copy-symbol ; Omit pointless secondary variant.
    copy-tree)
   t)
  ;; copy-readtable (todo)

  ((find find-if find-if-not
         member member-if member-if-not
         position position-if position-if-not
         remove remove-if remove-if-not
         delete delete-if delete-if-not
         count count-if count-if-not)
   t 2)

  ;; decode-float (3 return values)
  ;; (decode-universal-time t) (many return values)

  ((delete-duplicates remove-duplicates) t)

  (delete-package t)
  ((numerator denominator) t)
  ((deposit-field dpb) t 3)
  (destructuring-bind t 2)
  ((digit-char digit-char-p) t)
  (directory t)
  ((namestring file-namestring directory-namestring host-namestring)
   t)
  (enough-namestring t)
  (documentation nil)
  ((echo-stream-input-stream echo-stream-output-stream) nil)
  (endp t)
  ;; (ensure-directories-exist t)
  (ensure-generic-function t)
  ((eq eql equal equalp) t)
  ((evenp oddp) t)
  ((every notany notevery some) t 2)
  (exp t)
  (expt t)
  (fboundp nil)
  (fdefinition nil)
  ((file-author file-write-date file-length) nil)
  (file-error-pathname nil)
  ;; file-position (Somewhat problematic)
  (file-string-length nil 2)
  (fill-pointer nil)
  (find-all-symbols t)
  (find-class nil)
  (find-method nil)
  (find-package nil)
  (find-restart nil)
  ((find-symbol intern) nil) ; Ignore second return value.
  (float t)
  ((float-digits float-precision float-radix)
   t)
  (float-sign t)
  (floatp t)
  (function-keywords t) ; Ignore second return value.
  (function-lambda-expression t) ; Ignore second and third return values.
  (functionp t)
  ((gcd lcm) t)
  (gensym t)
  (get nil)
  (get-macro-character nil)
  (get-output-stream-string t)
  ;; (get-properties t) (3 return values)
  ;; get-setf-expansion (5 return values)
  (getf nil)
  (gethash nil 2) ; Ignore second return value.
  (graphic-char-p nil)
  ;; handler-case
  ((hash-table-count hash-table-p) t)
  ((hash-table-rehash-size
    hash-table-rehash-threshold
    hash-table-size
    hash-table-test)
   nil)
  (identity t)
  ((realpart imagpart) nil)
  (phase t)
  ((input-stream-p output-stream-p) t)
  ;; (integer-decode-float t) (3 return values)
  (integer-length t)
  (integerp t)
  (interactive-stream-p t)
  ((intersection set-difference set-exclusive-or union subsetp
                 nintersection nset-difference nset-exclusive-or nunion)
   t)
  (invoke-restart t) ; Ignore all but first return value.
  (invoke-restart-interactively t) ; Ignore all but first return value.
  ((sqrt isqrt) t)
  (keywordp t)
  (last nil)
  ((ldb ldb-test mask-field) nil 2)
  (ldiff t)
  ((length list-length) t)
  ((list vector) t)
  (listen t)
  (listp t)
  (load t)
  (load-logical-pathname-translations t)
  (log t)
  ((logand lognand logandc1 logandc2 logeqv
           logior lognor logorc1 logorc2 logxor logtest)
   t)
  (lognot t)
  (logbitp t 2)
  (logcount t)
  (logical-pathname t)
  (logical-pathname-translations nil)
  ;; Omit pointless secondary variant and ignore second return value.
  ((macroexpand macroexpand-1) t)
  (make-array t)
  ((make-broadcast-stream make-concatenated-stream) t)
  (make-condition t)
  (make-echo-stream t)
  (make-instance t)
  (make-list t)
  ;; Omit pointless secondary variant and ignore second return value.
  (make-load-form t)
  (make-load-form-saving-slots t) ; Ignore second return value.
  (make-package t)
  (make-random-state t)
  (make-sequence t 2)
  (make-string t)
  (make-string-input-stream t)
  (make-symbol t)
  (make-synonym-stream t)
  (make-two-way-stream t)
  ((map map-into) t 3)
  ((mapcar mapcan maplist mapcon) t 2)
  ((min max) t)
  (merge t 2)
  (merge-pathnames t)
  (method-qualifiers nil)
  ((minusp plusp zerop) t)
  (mismatch t)
  ((mod rem) t)
  ;; multiple-value-call (?...)
  (name-char t)
  ;; no-applicable-method no-next-method
  ;; ((nreconc revappend) t ?)
  ((reverse nreverse) t)
  ((string-capitalize string-downcase string-upcase
                      nstring-capitalize nstring-downcase nstring-upcase)
   t)
  ((sublis nsublis) t 2)
  ((subst subst-if subst-if-not nsubst nsubst-if nsubst-if-not
          substitute substitute-if substitute-if-not
          nsubstitute nsubstitute-if nsubstitute-if-not)
   t 3)
  ((nth nthcdr) nil 2)
  (null t)
  (numberp t)
  (open t)
  (open-stream-p nil)
  (package-error-package nil)
  ((package-name
    package-nicknames
    package-shadowing-symbols
    package-use-list
    package-used-by-list)
   nil)
  (packagep t)
  (pairlis t 3)
  (parse-integer t) ; Ignore second return value.
  (parse-namestring t) ; Ignore second return value.
  ((pathname pathnamep) t)
  ((pathname-device pathname-directory pathname-host
                    pathname-name pathname-type pathname-version)
   nil)
  (pathname-match-p t)
  ;; (pprint-dispatch t ?) 1 or 2?
  (pprint-fill t 2) (pprint-linear t 2) (pprint-tabular t 2)
  ((write prin1 princ print) t) ; To facilitate debugging.
  ((write-to-string prin1-to-string princ-to-string) t)
  (print-not-readable-object nil)
  ((probe-file translate-logical-pathname translate-pathname truename)
   t)
  ;; (random t ?) 1 or 2?
  (random-state-p t)
  ((rational rationalize) t)
  (rationalp t)
  ((read read-preserving-whitespace read-byte read-char read-char-no-hang)
   t)
  (read-delimited-list t 2)
  ((read-from-string read-line) t) ; Ignore second return value.
  ;; (read-sequence t ?) 1 or 2?
  (readtable-case nil)
  (readtablep t)
  (realp t)
  (reduce t 2)
  (remhash t 2)
  ;; rename-file
  ((search replace) t)
  (rest nil)
  (restart-name nil)
  ;; rotate
  ;; shift
  (scale-float t)
  (signum t)
  (simple-bit-vector-p t)
  ((simple-condition-format-arguments simple-condition-format-control)
   nil)
  (simple-string-p t)
  (simple-vector-p t)
  ((slot-boundp slot-exists-p slot-value) nil)
  ;; ((sort stable-sort) t) No point...
  (special-operator-p nil)
  (standard-char-p t)
  ((stream-element-type stream-external-format) nil)
  (stream-error-stream nil)
  (streamp t)
  (string t)
  ((string-not-equal string-lessp string-not-greaterp
                     string-equal string-not-lessp string-greaterp
                     string/= string< string<= string= string>= string>)
   t)
  ((string-left-trim string-right-trim string-trim) t 2)
  (stringp t)
  (subseq t)
  (subtypep t) ; Ignore second return value.
  (sxhash t)
  ((symbol-function symbol-name symbol-package symbol-plist symbol-value)
   nil)
  (symbolp nil)
  (synonym-stream-symbol nil)
  (tailp t 2)
  ;; the: todo
  (tree-equal t)
  ((two-way-stream-input-stream two-way-stream-output-stream) t)
  ((type-error-datum type-error-expected-type) nil)
  (typep t)
  (unbound-slot-instance nil)
  ;(unintern t ?)
  ;; Todo: Leave as-is without writeback when test failure.
  ;; (when unless)
  ((upgraded-array-element-type upgraded-complex-part-type) t)
  (user-homedir-pathname nil)
  (vector-pop t)
  ;; ((vector-push vector-push-extend) t ?) 2 or 1?
  (vectorp t)
  (wild-pathname-p nil))
