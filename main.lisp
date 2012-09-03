(in-package #:place-modifiers)

(defun place-modifier:parse-operator (name/variant &key errorp)
  (typecase name/variant
    ((and (not null) symbol)
     (values name/variant 0))
    ((cons (and (not null) symbol) (cons t null))
     (let ((found (position (second name/variant) '(* ** ***))))
       (if found
           (values (first name/variant) (1+ found))
           (values nil nil))))
    (t (if errorp
           (error "~S is not a valid place-modifier operator."
                  name/variant)
           (values nil nil)))))

(defun %actual-spot-index (args-count spot-index name)
  (cond
    ((minusp spot-index)
     (unless (>= args-count (abs spot-index))
       (error "Place-modifier ~S specifies place in ~S (from end) position ~
               and so requires at least this many args, ~S passed."
              name
              (1+ spot-index)
              args-count))
     (+ args-count spot-index))
    (t
     (unless (> args-count spot-index)
       (error "Place-modifier ~S specifies place in ~S position ~
               and so requires at least this many args, ~S passed."
              name
              (1+ spot-index)
              args-count))
     spot-index)))

(defun %split-at-index (sequence index)
  (values (subseq sequence 0 index)
          (elt sequence index)
          (subseq sequence (1+ index))))

(defun %split (pm-info variant args place-modification-expression)
  (check-type args list)
  (%split-at-index
   args
   (%actual-spot-index
    (let ((c (length args)))
      (when (zerop c)
        (error "All place modification expressions need ~
                at least 1 arg. ~S doesn't qualify."
               place-modification-expression))
      c)
    (place-modifier:spot-index pm-info variant)
    (place-modifier:names pm-info))))

(defun %augment-template (base-template operator before-spot after-spot)
  (lambda (fill-in)
    (funcall base-template
             `(,operator ,@before-spot ,fill-in ,@after-spot))))

(defun %finish (template place env oldp)
  (multiple-value-bind (vars vals stores writer reader)
      (get-setf-expansion place env)
    (when (/= (length stores) 1)
      (error "~S only supports places with exactly ~
              one store variable but place ~S has ~D: ~S."
             'modify place (length stores) stores))
    (let* ((old-var (and oldp (gensym (string '#:old))))
           (wrapped-old-var (and old-var (list old-var))))
      `(let* (,@vars ,@wrapped-old-var)
         (multiple-value-bind ,stores
             ,(funcall
               template
               (let ((vars-vals-plist (mapcan #'list vars vals)))
                 (if oldp
                     `(setf ,@vars-vals-plist
                            ,old-var ,reader)
                     `(progn (setf ,@vars-vals-plist)
                             ,reader))))
           ,writer
           ,@wrapped-old-var)))))

(defun nop (&rest args)
  (declare (ignore args))
  (values))

(defun %analyze (pme-or-place
                 &key (on-pme #'nop) (on-place #'nop) (on-ambiguous #'nop)
                 (on-place-marker #'nop))
  (etypecase pme-or-place
    (atom
     (funcall on-place pme-or-place))
    ((cons (eql :place))
     (destructuring-bind (place) (rest pme-or-place)
       (funcall on-place-marker place)))
    (list
     (destructuring-bind (operator &rest args) pme-or-place
       (multiple-value-bind (pm-name variant)
           (place-modifier:parse-operator operator)
         (let ((pm-info (and pm-name (place-modifier:locate pm-name :errorp nil))))
           (if (place-modifier:inconceivable-place-p pm-info)
               (multiple-value-call on-pme pme-or-place pm-info pm-name variant
                                    (%split pm-info variant args pme-or-place))
               (funcall on-ambiguous pme-or-place pm-info variant))))))))

(defun %augmented-template-caller (function template)
  (lambda (pme pm-info name variant before-spot spot after-spot)
    (declare (ignore pme pm-info variant))
    (funcall function spot
             (%augment-template template name before-spot after-spot))))

(defun %walk-conservatively-non-top-level (pme-or-place)
  (labels ((recurse (pme-or-place template)
             (let ((found-place (lambda (place)
                                  (values t place template))))
               (%analyze
                pme-or-place
                :on-pme
                (%augmented-template-caller #'recurse template)
                :on-place found-place
                :on-place-marker found-place
                :on-ambiguous
                (lambda (ambiguous pm-info variant)
                  (declare (ignore pm-info variant))
                  (multiple-value-bind (speculation-successful-p
                                        explicit-place
                                        full-template)
                      (%walk-speculatively-non-top-level ambiguous template)
                    (if speculation-successful-p
                        (values t explicit-place full-template)
                        (values t ambiguous template))))))))
    (recurse pme-or-place #'identity)))

(defun %walk-speculatively-non-top-level (pme-or-place template)
  (labels ((recurse (pme-or-place template)
             (%analyze
              pme-or-place
              :on-place-marker (lambda (place)
                                 (values t place template))
              :on-place (lambda (place)
                          (declare (ignore place))
                          (values nil nil nil))
              :on-pme (%augmented-template-caller #'recurse template)
              :on-ambiguous
              (lambda (ambiguous pm-info variant)
                (multiple-value-bind (before-spot spot after-spot)
                    (%split pm-info variant (rest ambiguous) ambiguous)
                  (recurse spot
                           (%augment-template template
                                              (first ambiguous)
                                              before-spot
                                              after-spot)))))))
    (recurse pme-or-place template)))

(defun %expand (place-modification-expression env)
  (let* ((oldp (and (typep place-modification-expression
                           '(cons (eql :old)))
                    (destructuring-bind (pme) (cdr place-modification-expression)
                      (prog1 t
                        (setf place-modification-expression pme)))))
         (place-at-top-level
          (lambda (place)
            (error "Found the following place at top-level ~
                    instead of place-modification-expression:~%~S"
                   place)))
         (finish
          (lambda (successp &optional place template)
            (if successp
                (%finish template place env oldp)
                (%finish #'identity place-modification-expression env oldp)))))
    (%analyze place-modification-expression
              :on-pme
              (lambda (pme &rest other-args)
                (declare (ignore other-args))
                (multiple-value-call finish
                  (%walk-conservatively-non-top-level pme)))
              :on-ambiguous
              (lambda (ambiguous pm-info variant)
                (declare (ignore pm-info variant))
                (multiple-value-call finish
                  (%walk-speculatively-non-top-level ambiguous #'identity)))
              :on-place place-at-top-level
              :on-place-marker place-at-top-level)))

(defmacro modify (&rest place-modification-expressions &environment env)
  `(progn
     ,@(map-bind (mapcar) ((expression place-modification-expressions))
         (%expand expression env))))
