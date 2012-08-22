(in-package #:place-modifiers)

(defvar *infos* (make-hash-table :test 'equal))

(deftype place-modifier:name ()
  '(or symbol (cons symbol (cons (eql *) null))))

(defclass place-modifier:info ()
  ((%name :initarg :name
          :reader place-modifier:name
          :type place-modifier:name)
   (%place-index :initarg :place-index
                 :reader place-modifier:place-index)))

(defmethod print-object (info stream)
  (print-unreadable-object (info stream :type t)
    (format stream "~W ~W ~W"
            (place-modifier:name info)
            :place-index
            (place-modifier:place-index info))))

(defun place-modifier:locate (name &key (errorp t))
  (check-type name place-modifier:name)
  (or (gethash name *infos*)
      (when errorp
	(error "There is no place-modifier named ~S." name))))

(defun place-modifier:ensure (name &key (place-index 0))
  (setf (gethash name *infos*)
	(make-instance 'place-modifier:info :name name :place-index place-index)))

(defun %destructure-definition (definition)
  (etypecase definition
    (symbol
     (values definition 0))
    ((cons symbol (cons (eql *) null))
     (values definition -1))
    ((or (cons symbol (cons integer null))
         (cons (cons symbol (cons (eql *) null)) (cons integer null)))
     (destructuring-bind (name place-index) definition
       (values name (ecase (signum place-index)
                      (-1 place-index)
                      (0 (error "place-index is 1-based in ~S definitions."
                                'place-modifier:define))
                      (1 (1- place-index))))))))

(defmacro place-modifier:define (definition)
  (multiple-value-bind (name place-index) (%destructure-definition definition)
    `(place-modifier:ensure ',name :place-index ,place-index)))

(defun %pindex (args-count info)
  (let ((pm-pindex (place-modifier:place-index info)))
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

(defun %operator (place-modifier-name)
  (check-type place-modifier-name place-modifier:name)
  (if (listp place-modifier-name)
      (first place-modifier-name)
      place-modifier-name))

(defmacro modify (&rest place-modification-expressions &environment env)
  `(progn
     ,@(map-bind (mapcar) ((expression place-modification-expressions))
         (destructuring-bind (place-modifier-name &rest args) expression
           (let ((operator (%operator place-modifier-name)))
             (multiple-value-bind (before-place place after-place)
                 (%split-at-index
                  args
                  (%pindex (let ((c (length args)))
                             (when (zerop c)
                               (error "All place modification expressions need ~
                                       at least 1 arg. ~S doesn't qualify."
                                      expression))
                             c)
                           (place-modifier:locate place-modifier-name)))
               (multiple-value-bind (vars vals stores writer reader)
                   (get-setf-expansion place env)
                 `(let* ,vars
                    (multiple-value-bind ,stores
                        (,operator
                         ,@before-place
                         (progn (setf ,@(mapcan #'list vars vals))
                                ,reader)
                         ,@after-place)
                      ,writer)))))))))

