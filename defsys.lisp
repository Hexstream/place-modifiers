(in-package #:place-modifiers)

(defclass definitions-system (defsys:standard-system)
  ())

(defvar *definitions* (make-instance 'definitions-system))

(setf (defsys:locate (defsys:root-system) 'modify)
      *definitions*)

(defparameter *spot-index-format* :human)

(defgeneric names (object))
(defgeneric inconceivable-place-p (object))
(defgeneric default-spot-index (definition &key))

(defclass definition () ())

(defclass standard-definition (definition)
  ((%names :initarg :names
           :reader names
           :type list)
   (%inconceivable-place-p :initarg :inconceivable-place-p
                           :reader inconceivable-place-p
                           :type boolean)
   (%default-spot-index :initarg :default-spot-index
                        :type fixnum)))

(defun convert-spot-index (spot-index source-format destination-format)
  (check-type source-format (member :machine :human))
  (check-type destination-format (member :machine :human :preserve))
  (when (eq destination-format :preserve)
    (setf destination-format source-format))
  (funcall
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
   spot-index))

(defmethod default-spot-index ((definition standard-definition) &key (format :machine))
  (convert-spot-index (slot-value definition '%default-spot-index)
                                     :machine
                                     format))

(defmethod initialize-instance :around ((definition standard-definition)
                                        &key names
                                        inconceivable-place-p
                                        default-spot-index
                                        spot-index-format)
  (call-next-method
   definition
   :names names
   :inconceivable-place-p inconceivable-place-p
   :default-spot-index (convert-spot-index default-spot-index spot-index-format :machine)))

(defmethod print-object ((definition standard-definition) stream)
  (print-unreadable-object (definition stream :type t)
    (let ((format *spot-index-format*))
      (format stream "~W ~C~W ~W"
              (names definition)
              (ecase format
                (:human #\h)
                (:machine #\m))
              (default-spot-index definition :format format)
              (if (inconceivable-place-p definition)
                  :inconceivable-place
                  :possible-place)))))

(defun %ensure (names inconceivable-place-p
                &key (default-spot-index 0) (spot-index-format :machine))
  (setf names (etypecase names
                (list names)
                (symbol (list names))))
  (let ((new (make-instance 'standard-definition
                            :names names
                            :inconceivable-place-p inconceivable-place-p
                            :default-spot-index default-spot-index
                            :spot-index-format spot-index-format)))
    (map-bind (mapcar) ((name names))
      (setf (defsys:locate *definitions* name) new))))

(defmacro define ((&key (spot-index-format :human)) &body definitions)
  `(progn
     ,@(map-bind (mapcar) ((definition definitions))
                 (destructuring-bind (names inconceivable-place-p
                                            &optional (default-spot-index 1))
                     definition
                   `(%ensure ',names ,inconceivable-place-p
                             :default-spot-index ',default-spot-index
                             :spot-index-format ,spot-index-format)))))
