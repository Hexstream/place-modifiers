(in-package #:place-modifiers)

(defvar *infos* (make-hash-table :test 'equal))

(deftype place-modifier:name ()
  '(or
    (and (not null) symbol)
    (cons (and (not null) symbol) (cons (eql *) null))))

(defclass place-modifier:info ()
  ((%name :initarg :name
          :reader place-modifier:name
          :type place-modifier:name)
   (%spot-index :initarg :spot-index
                :reader place-modifier:spot-index)))

(defmethod print-object (info stream)
  (print-unreadable-object (info stream :type t)
    (format stream "~W ~W ~W"
            (place-modifier:name info)
            :spot-index
            (place-modifier:spot-index info))))

(defun place-modifier:locate (name &key (errorp t))
  (check-type name place-modifier:name)
  (or (gethash name *infos*)
      (when errorp
	(error "There is no place-modifier named ~S." name))))

(defun place-modifier:ensure (name &key (spot-index 0))
  (setf (gethash name *infos*)
	(make-instance 'place-modifier:info :name name :spot-index spot-index)))

(defun %destructure-definition (definition)
  (etypecase definition
    (symbol
     (values definition 0))
    ((cons symbol (cons (eql *) null))
     (values definition -1))
    ((or (cons symbol (cons integer null))
         (cons (cons symbol (cons (eql *) null)) (cons integer null)))
     (destructuring-bind (name spot-index) definition
       (values name (ecase (signum spot-index)
                      (-1 spot-index)
                      (0 (error "spot-index is 1-based in ~S definitions."
                                'place-modifier:define))
                      (1 (1- spot-index))))))))

(defmacro place-modifier:define (definition)
  (multiple-value-bind (name spot-index) (%destructure-definition definition)
    `(place-modifier:ensure ',name :spot-index ,spot-index)))

(defun %pindex (args-count info)
  (let ((pm-pindex (place-modifier:spot-index info)))
    (cond
      ((minusp pm-pindex)
       (unless (>= args-count (abs pm-pindex))
         (error "Place-modifier ~S specifies place in ~S (from end) position ~
                 and so requires at least this many args, ~S passed."
                (place-modifier:name info)
                (1+ pm-pindex)
                args-count))
       (+ args-count pm-pindex))
      (t
       (unless (> args-count pm-pindex)
         (error "Place-modifier ~S specifies place in ~S position ~
                 and so requires at least this many args, ~S passed."
                (place-modifier:name info)
                (1+ pm-pindex)
                args-count))
       pm-pindex))))

(defun %split-at-index (sequence index)
  (values (subseq sequence 0 index)
          (elt sequence index)
          (subseq sequence (1+ index))))

(defun %potential-place-modifier-name (potential-name)
  (and (typep potential-name 'place-modifier:name)
       (if (listp potential-name)
           (first potential-name)
           potential-name)))

(defun %expand (place-modification-expression env)
  (labels
    ((recurse (pme-or-place top-level-p template)
       (destructuring-bind (operator &rest args) (if (listp pme-or-place)
                                                     pme-or-place
                                                     (list pme-or-place))
         (let ((pm-info
                (let ((pm-name (%potential-place-modifier-name operator)))
                  (and pm-name
                       (place-modifier:locate pm-name :errorp nil)))))
           (cond
             (pm-info ; spot is a place modification expression
              (multiple-value-bind (before-spot spot after-spot)
                  (%split-at-index
                   args
                   (%pindex (let ((c (length args)))
                              (when (zerop c)
                                (error "All place modification expressions need ~
                                        at least 1 arg. ~S doesn't qualify."
                                       pme-or-place))
                              c)
                            pm-info))
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
                     ,writer)))))))))
    (recurse place-modification-expression t #'identity)))

(defmacro modify (&rest place-modification-expressions &environment env)
  `(progn
     ,@(map-bind (mapcar) ((expression place-modification-expressions))
         (%expand expression env))))
