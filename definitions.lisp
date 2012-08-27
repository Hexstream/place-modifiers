(in-package #:place-modifiers)

(place-modifier:define ()
  ((* + - /) t (1 -1))
  ((1+ 1-) t)
  ((< <= = /= > >=) t (1 -1))
  (abs t)
  (acons t (3 1 2))
  ((sin cos tan
        asin acos ;atan (see below)
        sinh cosh tanh
        asinh acosh atanh
        cis)
   t)
  (atan t (1 2))
  (adjoin t (2 1))
  (adjust-array t (1 2))
  (adjustable-array-p nil)
  (allocate-instance t)
  (alpha-char-p t)
  (alphanumericp 1)
  ((and or) t (1 -1))
  ((append nconc list*) t (-1 1))
  (apply nil (-1 2 1))
  (funcall nil (2 -1 1))
  (apropos-list t (1 2))
  (aref nil (1 2 -1))
  ((elt svref) nil (1 2))
  ((arithmetic-error-operands arithmetic-error-operation) nil)
  (array-dimension nil (1 2))
  (array-dimensions nil)
  (array-displacement nil) ; Ignore second return value.
  ((array-element-type
    array-has-fill-pointer-p
    array-in-bounds-p
    array-rank)
   nil)
  (array-row-major-index nil (1 2 -1))
  (array-total-size nil)
  (arrayp t)
  (ash t (1 2))
  ((assoc assoc-if assoc-if-not) nil (2 1))
  (atan t (1 2)) (atanh t 1)
  (atom t)
  ((bit sbit) nil (1 2 -1))
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
   t (1 2 3))
  (bit-not t (1 2))
  (bit-vector-p t)
  (boole t (2 3 1)))

#+nil(define-modify-macro (modify boolean) (&place generalized-boolean) ; caution
       (lambda (generalized-boolean)
         (if generalized-boolean t nil)))

(place-modifier:define ()
  ((both-case-p lower-case-p upper-case-p) t)
  (boundp nil)
  (broadcast-stream-streams nil)
  ((butlast nbutlast) nil (1 2))
  (byte t (1 2))
  ((byte-position byte-size) nil)
  ((car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
        caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
        cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
   nil)
  ((first second third fourth fifth sixth seventh eighth ninth tenth)
   nil)

  ((case ccase ecase typecase ctypecase etypecase)
   t)

  ((ceiling fceiling floor ffloor round fround truncate ftruncate)
   t (1 2)) ; Ignore second return value.
  (cell-error-name nil)
  ((char schar) nil (1 2))
  ((char-code code-char char-int char-name char-downcase char-upcase)
   t)
  ((char-equal char-greaterp char-lessp
               char-not-equal char-not-greaterp char-not-lessp
               char/= char< char<= char= char> char>=)
   t (1 -1))
  ((character characterp) t)
  (class-name t)
  (class-of nil)
  (close t (1 2))
  (coerce t (1 2)))

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
  (complex t (1 2))
  (complexp t)
  (compute-applicable-methods t (1 2))
  (compute-restarts t)
  (concatenate t (2 -1 1))
  (concatenated-stream-streams nil)
  (conjugate t)
  (cons t (2 1))
  (consp t)
  (constantly t)
  (constantp t (1 2))
  ((copy-alist
    copy-list
    copy-pprint-dispatch
    copy-seq
    copy-structure
    copy-symbol ; Omit pointless secondary variant.
    copy-tree)
   t)
  ; copy-readtable (todo)

  ((find find-if find-if-not
         member member-if member-if-not
         position position-if position-if-not
         remove remove-if remove-if-not
         delete delete-if delete-if-not
         count count-if count-if-not)
   t (2 1))

  ; decode-float (3 return values)
  ; (decode-universal-time t) (many return values)

  ((delete-duplicates remove-duplicates) t)

  (delete-package t)
  ((numerator denominator) t)
  ((deposit-field dpb) t (3 2 1))
  (destructuring-bind t 2)
  ((digit-char digit-char-p) t (1 2))
  (directory t 1)
  ((namestring file-namestring directory-namestring host-namestring)
   t)
  (enough-namestring t (1 2))
  (documentation nil (1 2))
  ((echo-stream-input-stream echo-stream-output-stream) nil)
  (endp t)
  ; (ensure-directories-exist t)
  (ensure-generic-function t)
  ((eq eql equal equalp) t (1 2))
  ((evenp oddp) t)
  ((every notany notevery some) t (2 -1 1))
  (exp t 1)
  (expt t (1 2))
  (fboundp nil)
  (fdefinition nil)
  ((file-author file-write-date file-length) nil)
  (file-error-pathname nil)
  ; file-position (Somewhat problematic)
  (file-string-length nil (2 1))
  (fill-pointer nil)
  (find-all-symbols t)
  (find-class nil)
  (find-method nil)
  (find-package nil)
  (find-restart nil)
  ((find-symbol intern) nil (1 2)) ; Ignore second return value.
  (float t (1 2))
  ((float-digits float-precision float-radix)
   t)
  (float-sign t (1 2))
  (floatp t 1)
  (function-keywords t) ; Ignore second return value.
  (function-lambda-expression t) ; Ignore second and third return values.
  (functionp t)
  ((gcd lcm) t (1 -1))
  (gensym t)
  (get nil (1 2))
  (get-macro-character nil (1 2))
  (get-output-stream-string t)
  ; (get-properties t) (3 return values)
  ; get-setf-expansion (5 return values)
  (getf nil (1 2 3))
  (gethash nil (2 1 3)) ; Ignore second return value.
  (graphic-char-p nil)
  ; handler-case
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
  ; (integer-decode-float t) (3 return values)
  (integer-length t)
  (integerp t)
  (interactive-stream-p t)
  ((intersection set-difference set-exclusive-or union
                 nintersection nset-difference nset-exclusive-or nunion)
   t (1 2))
  (invoke-restart t (1 2 -1)) ; Ignore all but first return value.
  (invoke-restart-interactively t) ; Ignore all but first return value.
  ((sqrt isqrt) t)
  (keywordp t)
  (last nil (1 2))
  ((ldb ldb-test mask-field) nil (2 1))
  (ldiff t (1 2))
  ((length list-length) t)
  (list t (1 -1))
  (listen t)
  (listp t)
  (load t)
  (load-logical-pathname-translations t)
  (log t (1 2))
  ((logand lognand logandc1 logandc2 logeqv
           logior lognor logorc1 logorc2 logxor logtest)
   t (1 -1))
  (lognot t)
  (logbitp t (2 1))
  (logcount t)
  (logical-pathname t)
  (logical-pathname-translations nil)
  ;; Omit pointless secondary variant and ignore second return value.
  ((macroexpand macroexpand-1) t)
  (make-array t)
  ((make-broadcast-stream make-concatenated-stream) t (1 -1))
  (make-condition t)
  (make-echo-stream t (1 2))
  (make-instance t)
  (make-list t)
  ;; Omit pointless secondary variant and ignore second return value.
  (make-load-form t)
  (make-load-form-saving-slots t) ; Ignore second return value.
  (make-package t)
  (make-random-state t)
  (make-sequence t (2 1))
  (make-string t)
  (make-string-input-stream t)
  (make-symbol t)
  (make-synonym-stream t)
  (make-two-way-stream t (1 2))
  ((map map-into) t (3 -1 2 1))
  ((mapcar mapcan maplist mapcon) t (2 -1 1))
  ((min max) t (1 -1))
  (merge t (2 3 1))
  (merge-pathnames t (1 2))
  (method-qualifiers nil)
  ((minusp plusp zerop) t)
  (mismatch t (1 2))
  ((mod rem) t (1 2))
  ; multiple-value-call (?...)
  (name-char t)
  ; no-applicable-method no-next-method
  ; ((nreconc revappend) t ?)
  ((reverse nreverse) t)
  ((string-capitalize string-downcase string-upcase
                      nstring-capitalize nstring-downcase nstring-upcase)
   t)
  (nsublis 2) ((nsublis *) 1)
  (nsubst 3) (nsubst-if 3) (nsubst-if-not 3)
  (nsubstitute 3) (nsubstitute-if 3) (nsubstitute-if-not 3)
  (nth 2) ((nth *) 1)
  (nthcdr 2) ((nthcdr *) 1)
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
  (pairlis t (3 1 2))
  (parse-integer t) ; Ignore second return value.
  (parse-namestring t) ; Ignore second return value.
  ((pathname pathnamep) t)
  ((pathname-device pathname-directory pathname-host
                    pathname-name pathname-type pathname-version)
   nil)
  (pathname-match-p t (1 2))
  (pprint-dispatch 2) ((pprint-dispatch *) 1)
  (pprint-fill 2) (pprint-linear 2) (pprint-tabular 2)
  prin1 prin1-to-string
  princ princ-to-string
  print-not-readable-object
  probe-file
  (random 2) ((random *) 1)
  random-state-p
  (rassoc 2) ((rassoc *) 1) (rassoc-if 2) (rassoc-if-not 2)
  rational rationalize
  rationalp
  read
  read-byte
  read-char
  read-char-no-hang
  (read-delimited-list 2) ((read-delimited-list *) 1)
  read-from-string
  read-line
  read-preserving-whitespace
  read-sequence
  readtable-case
  readtablep
  realp
  (reduce 2)
  (remhash 2) ((remhash *) 1)
  (remove 2) ((remove *) 1) (remove-if 2) (remove-if-not 2)
  remove-duplicates
  rename-file (rename-file *)
  rename-package
  replace ((replace *) 2)
  rest
  restart-name
                                        ;(rotate &rest 0)
                                        ;(rotate* &rest 0)
                                        ;(shift &rest ?)
                                        ;(shift* (&rest 2))
  row-major-aref (row-major-aref *)
  scale-float (scale-float *)
  search ((search *) 2)
  set
  signum
  simple-bit-vector-p
  simple-condition-format-arguments
  simple-condition-format-control
  simple-string-p
  simple-vector-p
  sin sinh
  slot-boundp (slot-boundp *)
  slot-exists-p (slot-exists-p *)
  slot-value (slot-value *)
  sort
  special-operator-p
  stable-sort
  standard-char-p
  stream-element-type
  stream-error-stream
  stream-external-format
  streamp
  string
  string-equal ((string-equal *) 2)
  string-greaterp ((string-greaterp *) 2)
  (string-left-trim 2) ((string-left-trim *) 1)
  (string-right-trim 2) ((string-right-trim *) 1)
  (string-trim 2) ((string-trim *) 1)
  string-lessp ((string-lessp *) 2)
  string-not-equal ((string-not-equal *) 2)
  string-not-greaterp ((string-not-greaterp *) 2)
  string-not-lessp ((string-not-lessp *) 2)
  string/= ((string/= *) 2)
  string< ((string< *) 2)
  string<= ((string<= *) 2)
  string= ((string= *) 2)
  string> ((string> *) 2)
  string>= ((string>= *) 2)
  stringp
  (sublis 2) ((sublis *) 1)
  subseq
  subsetp ((subsetp *) 2)
  (subst 3) (subst-if 3) (subst-if-not 3)
  (substitute 3) (substitute-if 3) (substitute-if-not 3)
  subtypep ((subtypep *) 2)
  sxhash
  symbol-function
  symbol-name
  symbol-package
  symbol-plist
  symbol-value
  symbolp
  synonym-stream-symbol
  (tailp 2) ((tailp *) 1)
  tan tanh
                                        ; the: todo
  translate-logical-pathname
  translate-pathname
  tree-equal ((tree-equal *) 2)
  truename
  two-way-stream-input-stream
  two-way-stream-output-stream
  type-error-datum
  type-error-expected-type
  type-of
  typep ((typep *) 2)
  unbound-slot-instance
  unintern ((unintern *) 2)
  union ((union *) 2)
                                        ;unless todo
                                        ;when todo
  upgraded-array-element-type
  upgraded-complex-part-type
  user-homedir-pathname
  vector (vector *)
  vector-pop
  (vector-push 2) ((vector-push *) 1)
  (vector-push-extend 2) ((vector-push-extend *) 1)
  vectorp
  wild-pathname-p
  write
  write-to-string)
