(in-package #:place-modifiers)

(defvar *infos* (make-hash-table :test 'eq))
(defparameter place-modifier:*spot-index-format* :human)

(defun map-infos (function)
  (maphash function *infos*))

(defgeneric place-modifier:names (object))
(defgeneric place-modifier:inconceivable-place-p (object))
(defgeneric place-modifier:spot-indexes (object &key))
(defgeneric place-modifier:spot-index (info variant))

(defclass place-modifier:info () ())

(defclass place-modifier:standard-info (place-modifier:info)
  ((%names :initarg :names
           :reader place-modifier:names
           :type list)
   (%inconceivable-place-p :initarg :inconceivable-place-p
                           :reader inconceivable-place-p
                           :type boolean)
   (%spot-indexes :initarg :spot-indexes
                  :type list)))

(defun %plurality-aware-map (function list-or-atom destination-plurality-format
                             &key forcep)
  (check-type destination-plurality-format (member :list :atom :preserve))
  (multiple-value-bind (source-plurality-format source-pluralp)
      (if (listp list-or-atom)
          (values :list (cddr list-or-atom))
          (values :atom nil))
    (when (eq destination-plurality-format :preserve)
      (setf destination-plurality-format source-plurality-format))
    (when source-pluralp
      (when (and forcep (eq destination-plurality-format :atom))
        (error "Can't forcibly map ~S into an atom ~
                because there's more than 1 element."
               list-or-atom))
      (setf destination-plurality-format :list))
    (cartesian-product-switch ((ecase source-plurality-format
                                 :list :atom)
                               (ecase destination-plurality-format
                                 :list :atom))
      (mapcar function list-or-atom)
      (funcall function (first list-or-atom))
      (list (funcall function list-or-atom))
      (funcall function list-or-atom))))

(defun place-modifier:convert-spot-indexes
    (spot-indexes source-format destination-format
     &key ((:plurality-format destination-plurality-format) :preserve))
  (check-type source-format (member :machine :human))
  (check-type destination-format (member :machine :human :preserve))
  (when (eq destination-format :preserve)
    (setf destination-format source-format))
  (%plurality-aware-map
   (if (eq source-format destination-format)
       #'identity
       (ecase source-format
         (:machine
          (lambda (spot-index)
            (if (minusp spot-index)
                spot-index
                (1+ spot-index))))
         (:human
          (lambda (spot-index)
            (ecase (signum spot-index)
              (-1 spot-index)
              (0 (error "spot-index of 0 is invalid in ~S format." :human))
              (1 (1- spot-index)))))))
   spot-indexes
   destination-plurality-format))

(defmethod place-modifier:spot-indexes ((info place-modifier:standard-info)
                                        &key (format :machine)
                                        (plurality-format :list))
  (place-modifier:convert-spot-indexes (slot-value info '%spot-indexes)
                                       :machine
                                       format
                                       :plurality-format plurality-format))

(defmethod place-modifier:spot-index ((info place-modifier:standard-info) variant)
  (nth variant (place-modifier:spot-indexes info)))

(defmethod initialize-instance :around ((info place-modifier:standard-info)
                                        &key names
                                        inconceivable-place-p
                                        spot-indexes
                                        spot-index-format)
  (call-next-method
   info
   :names names
   :inconceivable-place-p inconceivable-place-p
   :spot-indexes (convert-spot-indexes spot-indexes
                                       spot-index-format
                                       :machine
                                       :plurality-format :list)))

(defmethod print-object ((info place-modifier:standard-info) stream)
  (print-unreadable-object (info stream :type t)
    (let ((*print-length* 3)
          (format place-modifier:*spot-index-format*))
      (format stream "~W ~C~W ~W"
              (place-modifier:names info)
              (ecase format
                (:human #\h)
                (:machine #\m))
              (place-modifier:spot-indexes info :format format)
              (if (place-modifier:inconceivable-place-p info)
                  :inconceivable-place
                  :possible-place)))))

(defun place-modifier:locate (name &key (errorp t))
  (check-type name (and symbol (not null)))
  (or (gethash name *infos*)
      (when errorp
	(error "There is no place-modifier named ~S." name))))

(defun place-modifier:ensure (names inconceivable-place-p
                              &key (spot-indexes 0) (spot-index-format :machine))
  (setf names (etypecase names
                (list names)
                (symbol (list names))))
  (let ((new (make-instance 'place-modifier:standard-info
                            :names names
                            :inconceivable-place-p inconceivable-place-p
                            :spot-indexes spot-indexes
                            :spot-index-format spot-index-format)))
    (map-bind (mapcar) ((name names))
      (setf (gethash name *infos*) new))))

(defmacro place-modifier:define ((&key (spot-index-format :human))
                                 &body definitions)
  `(progn
     ,@(map-bind (mapcar) ((definition definitions))
         (destructuring-bind (names inconceivable-place-p
                                    &optional (spot-indexes 1))
             definition
           `(place-modifier:ensure ',names ,inconceivable-place-p
                                   :spot-indexes ',spot-indexes
                                   :spot-index-format ,spot-index-format)))))
