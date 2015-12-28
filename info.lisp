(in-package #:place-modifiers)

(defvar *infos* (make-hash-table :test 'eq))
(defparameter place-modifier:*spot-index-format* :human)

(defun map-infos (function)
  (maphash function *infos*))

(defgeneric place-modifier:names (object))
(defgeneric place-modifier:inconceivable-place-p (object))
(defgeneric place-modifier:default-spot-index (info &key))

(defclass place-modifier:info () ())

(defclass place-modifier:standard-info (place-modifier:info)
  ((%names :initarg :names
           :reader place-modifier:names
           :type list)
   (%inconceivable-place-p :initarg :inconceivable-place-p
                           :reader inconceivable-place-p
                           :type boolean)
   (%default-spot-index :initarg :default-spot-index
                        :type fixnum)))

(defun place-modifier:convert-spot-index (spot-index source-format destination-format)
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

(defmethod place-modifier:default-spot-index ((info place-modifier:standard-info) &key (format :machine))
  (place-modifier:convert-spot-index (slot-value info '%default-spot-index)
                                     :machine
                                     format))

(defmethod initialize-instance :around ((info place-modifier:standard-info)
                                        &key names
                                        inconceivable-place-p
                                        default-spot-index
                                        spot-index-format)
  (call-next-method
   info
   :names names
   :inconceivable-place-p inconceivable-place-p
   :default-spot-index (convert-spot-index default-spot-index spot-index-format :machine)))

(defmethod print-object ((info place-modifier:standard-info) stream)
  (print-unreadable-object (info stream :type t)
    (let ((format place-modifier:*spot-index-format*))
      (format stream "~W ~C~W ~W"
              (place-modifier:names info)
              (ecase format
                (:human #\h)
                (:machine #\m))
              (place-modifier:default-spot-index info :format format)
              (if (place-modifier:inconceivable-place-p info)
                  :inconceivable-place
                  :possible-place)))))

(defun place-modifier:locate (name &key (errorp t))
  (check-type name (and symbol (not null)))
  (or (gethash name *infos*)
      (when errorp
	(error "There is no place-modifier named ~S." name))))

(defun place-modifier:ensure (names inconceivable-place-p
                              &key (default-spot-index 0) (spot-index-format :machine))
  (setf names (etypecase names
                (list names)
                (symbol (list names))))
  (let ((new (make-instance 'place-modifier:standard-info
                            :names names
                            :inconceivable-place-p inconceivable-place-p
                            :default-spot-index default-spot-index
                            :spot-index-format spot-index-format)))
    (map-bind (mapcar) ((name names))
      (setf (gethash name *infos*) new))))

(defmacro place-modifier:define ((&key (spot-index-format :human))
                                 &body definitions)
  `(progn
     ,@(map-bind (mapcar) ((definition definitions))
         (destructuring-bind (names inconceivable-place-p
                                    &optional (default-spot-index 1))
             definition
           `(place-modifier:ensure ',names ,inconceivable-place-p
                                   :default-spot-index ',default-spot-index
                                   :spot-index-format ,spot-index-format)))))
