(in-package #:place-modifiers)

(defvar *infos* (make-hash-table :test 'eq))

(deftype place-modifier:name ()
  '(or
    (and (not null) symbol)
    (cons (and (not null) symbol) (cons (eql *) null))))

(defgeneric place-modifier:name (object))
(defgeneric place-modifier:inconceivable-place-p (object))
(defgeneric place-modifier:spot-indexes (object &optional format))
(defgeneric place-modifier:spot-index (info variant))

(defun place-modifier:parse-operator (name/variant)
  (case name/variant
    ((and (not null) symbol)
     (values name/variant 0))
    ((cons (and (not null) symbol) (cons t null))
     (let ((found (position (second name/variant) '(* ** ***))))
       (if found
           (values (first name/variant) (1+ found))
           (values nil nil))))
    (t (values nil nil))))

(defclass place-modifier:info () ())

(defclass place-modifier:standard-info (place-modifier:info)
  ((%names :reader place-modifier:name
           :type list)
   (%inconceivable-place-p :reader inconceivable-place-p
                           :type boolean)
   (%spot-indexes :type list)))

(defmethod place-modifier:spot-indexes ((info place-modifier:standard-info)
                                        &optional (format :machine))
  (let ((indexes (slot-value info '%spot-indexes)))
    (ecase format
      (:machine indexes)
      (:human (map-bind (mapcar) ((index indexes))
                (if (minusp index)
                    index
                    (1+ index)))))))

(defmethod place-modifier:spot-index ((info place-modifier:standard-info) variant)
  (nth (place-modifier:spot-indexes info) variant))

(defun %spot-index-human-to-machine (spot-index)
  (ecase (signum spot-index)
    (-1 spot-index)
    (0 (error "spot-index of 0 is invalid in ~S format." :human))
    (1 (1- spot-index))))

(defmethod initialize-instance :around ((info place-modifier:standard-info)
                                        &key names
                                        inconceivable-place-p
                                        spot-indexes
                                        spot-indexes-format)
  (setf spot-indexes (etypecase spot-indexes
                       (integer (list spot-indexes))
                       (list spot-indexes)))
  (call-next-method
   info
   :names names
   :inconceivable-place-p inconceivable-place-p
   :spot-indexes (ecase spot-indexes-format
                   (:machine spot-indexes)
                   (:human (mapcar #'%spot-index-human-to-machine spot-indexes)))))

(defmethod print-object ((info place-modifier:standard-info) stream)
  (print-unreadable-object (info stream :type t)
    (format stream "~W ~W ~W ~W"
            (place-modifier:name info)
            (if (inconceivable-place-p info)
                :inconceivable-place
                :possible-place)
            :spot-indexes
            (place-modifier:spot-indexes info))))

(defun place-modifier:locate (name &key (errorp t))
  (check-type name (and symbol (not null)))
  (or (gethash name *infos*)
      (when errorp
	(error "There is no place-modifier named ~S." name))))

(defun place-modifier:ensure (names inconceivable-place-p
                              &key (spot-indexes 0) (spot-indexes-format :machine))
  (setf names (etypecase names
                (list names)
                (symbol (list names))))
  (let ((new (make-instance 'place-modifier:standard-info
                            :names names
                            :inconceivable-place-p inconceivable-place-p
                            :spot-indexes spot-indexes
                            :spot-indexes-format spot-indexes-format)))
    (map-bind (mapcar) ((name names))
      (setf (gethash name *infos*) new))))

(defmacro place-modifier:define ((&key (spot-indexes-format :human))
                                 &rest definitions)
  `(progn
     ,@(map-bind (mapcar) ((definition definitions))
         (destructuring-bind (names inconceivable-place-p
                                    &optional (spot-indexes 1))
             definition
           `(place-modifier:ensure ',names ,inconceivable-place-p
                                   :spot-indexes ,spot-indexes
                                   :spot-indexes-format ,spot-indexes-format)))))

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

(defun %expand (place-modification-expression env)
  (labels
    ((recurse (pme-or-place top-level-p template)
       (destructuring-bind (operator &rest args) (if (listp pme-or-place)
                                                     pme-or-place
                                                     (list pme-or-place))
         (multiple-value-bind (pm-name variant) (place-modifier:parse-operator operator)
           (let ((pm-info
                  (and pm-name
                       (place-modifier:locate pm-name :errorp nil))))
             (cond
               (pm-info ; spot is a place modification expression
                (multiple-value-bind (before-spot spot after-spot)
                    (%split-at-index
                     args
                     (%actual-spot-index
                      (let ((c (length args)))
                        (when (zerop c)
                          (error "All place modification expressions need ~
                                  at least 1 arg. ~S doesn't qualify."
                                  pme-or-place))
                        c)
                      (place-modifier:spot-index pm-info variant)
                      (place-modifier:name pm-info)))
                  (recurse spot
                           nil
                           (lambda (fill-in)
                             (funcall template
                                      `(,operator ,@before-spot
                                                  ,fill-in
                                                  ,@after-spot))))))
               (t ; spot is a place
                (when top-level-p
                  (error "~S is not a valid place-modification-expression."
                         pme-or-place))
                (multiple-value-bind (vars vals stores writer reader)
                    (get-setf-expansion pme-or-place env)
                  `(let* ,vars
                     (multiple-value-bind ,stores
                         ,(funcall template
                                   `(progn (setf ,@(mapcan #'list vars vals))
                                           ,reader))
                       ,writer))))))))))
    (recurse place-modification-expression t #'identity)))

(defmacro modify (&rest place-modification-expressions &environment env)
  `(progn
     ,@(map-bind (mapcar) ((expression place-modification-expressions))
         (%expand expression env))))
