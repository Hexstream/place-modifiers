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
  (print place)
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
             ,(print (funcall
               template
               (print (let ((vars-vals-plist (mapcan #'list vars vals)))
                 (if oldp
                     `(setf ,@vars-vals-plist
                            ,old-var ,reader)
                     `(progn (setf ,@vars-vals-plist)
                             ,reader))))))
           ,writer
           ,@wrapped-old-var)))))

(defun %expand (place-modification-expression env
                &aux (oldp
                      (and (typep place-modification-expression
                                  '(cons (eql :old) (cons t null)))
                           (prog1 t
                             (setf place-modification-expression
                                   (second place-modification-expression)))))
                conservative-place)
  (labels
    ((not-valid (not-pme)
       (error "~S is not a valid place-modification-expression."
              not-pme))
     (recurse (pme-or-place state conservative-template speculative-template)
       (unless (consp pme-or-place)
         (when (eq state :top-level)
           (not-valid pme-or-place))
         (return-from recurse
           (%finish conservative-template conservative-place env oldp)))
       (destructuring-bind (operator &rest args) pme-or-place
         (multiple-value-bind (pm-name variant)
             (place-modifier:parse-operator operator)
           (let ((pm-info
                  (and pm-name
                       (place-modifier:locate pm-name :errorp nil))))
             (let ((top-level-p (eq state :top-level)))
               (when (and top-level-p (not pm-info))
                 (not-valid pme-or-place))
               (when (eq operator :place)
                 (return-from recurse
                   (%finish speculative-template (second pme-or-place) env oldp)))
               (when top-level-p
                 (setf state :conservative-search)))
             (multiple-value-bind (before-spot spot after-spot)
                 (and pm-info (%split pm-info variant args pme-or-place))
               (flet ((%continue (new-state)
                        (flet ((augment (template)
                                 (%augment-template template
                                                    operator
                                                    before-spot
                                                    after-spot)))
                          (recurse spot
                                   new-state
                                   (ecase state
                                     (:conservative-search
                                      (setf conservative-place pme-or-place)
                                      (augment conservative-template))
                                     (:speculative-search
                                      conservative-template))
                                   (augment speculative-template)))))
                 (cartesian-product-switch
                     ((ecase state :conservative-search :speculative-search)
                      (if pm-info))
                   ;; :conservative-search
                   (%continue (if (place-modifiers:inconceivable-place-p pm-info)
                                  :conservative-search
                                  :speculative-search))
                   (%continue :speculative-search) ; start speculative search
                   ;; :speculative-search
                   (%continue :speculative-search) ; continue speculative search
                   (%finish conservative-template conservative-place env oldp)))))))))
    (recurse place-modification-expression :top-level #'identity #'identity)))

(defmacro modify (&rest place-modification-expressions &environment env)
  `(progn
     ,@(map-bind (mapcar) ((expression place-modification-expressions))
         (%expand expression env))))
