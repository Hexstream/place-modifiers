(in-package #:place-modifiers)

(defun %actual-spot-index (args-count spot-index)
  (cond
    ((minusp spot-index)
     (when (>= args-count (abs spot-index))
       (+ args-count spot-index)))
    (t
     (when (> args-count spot-index)
       spot-index))))

(defun %actual-spot-value (list spot-index)
  (when spot-index
    (let ((actual-spot-index (%actual-spot-index (length list) spot-index)))
      (when actual-spot-index
        (nth actual-spot-index list)))))

#+nil(error "All place modification expressions need ~
             at least 1 arg. ~S doesn't qualify."
       place-modification-expression)

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
                     (if vars-vals-plist
                         `(progn (setf ,@vars-vals-plist)
                                 ,reader)
                         reader))))
           ,writer
           ,@wrapped-old-var)))))

(defun nop (&rest args)
  (declare (ignore args))
  (values))

(defun %analyze (pme-or-place &key
                                (on-place #'nop)
                                (on-pme #'nop)
                                (on-ambiguous #'nop))
  (etypecase pme-or-place
    (atom
     (funcall on-place pme-or-place :assumed))
    ((cons (eql :place))
     (destructuring-bind (place) (rest pme-or-place)
       (funcall on-place place :explicit)))
    (list
     (destructuring-bind (pm-name &rest args) pme-or-place
       (declare (ignore args))
       (let ((pm-info (and pm-name (place-modifier:locate pm-name :errorp nil))))
         (if pm-info
             (funcall (if (place-modifier:inconceivable-place-p pm-info)
                          on-pme
                          on-ambiguous)
                      pme-or-place)
             (funcall on-place pme-or-place :assumed)))))))

(defun %map-spots (function args)
  (maplist (lambda (tail)
             (funcall function
                      (ldiff args tail)
                      (first tail)
                      (rest tail)))
           args))

(defun %choose-best (results-list default-spot-index)
  (values-list
   (let ((explicit-place-results
          (remove :assumed results-list :key #'third)))
     (case (length explicit-place-results)
       (0 (%actual-spot-value results-list default-spot-index))
       (1 (first explicit-place-results))
       (t (error "Multiple (~A) explicit places found:~{~%~^~S~}"
                 (length explicit-place-results)
                 (mapcar #'second explicit-place-results)))))))

(defun %augmented-template-caller (function template)
  (lambda (pme)
    (let ((operator (first pme)))
      (%choose-best (%map-spots (lambda (before-spot spot after-spot)
                                  (multiple-value-list
                                   (funcall function spot
                                            (%augment-template template
                                                               operator
                                                               before-spot
                                                               after-spot))))
                                (rest pme))
                    (default-spot-index (place-modifier:locate operator))))))

(defun %make-speculative-walk-handler (function template)
  (lambda (ambiguous)
    (multiple-value-bind (full-template place kind)
        (%walk-speculatively-non-top-level ambiguous template)
      (multiple-value-call function
        (if (eq kind :explicit)
            (values full-template place)
            (values template ambiguous))
        kind))))

(defun %walk-conservatively-non-top-level (pme-or-place)
  (labels ((recurse (pme-or-place template)
             (%analyze
              pme-or-place
              :on-place (lambda (place kind)
                          (values template place kind))
              :on-pme (%augmented-template-caller #'recurse template)
              :on-ambiguous (%make-speculative-walk-handler #'values template))))
    (recurse pme-or-place #'identity)))

(defun %walk-speculatively-non-top-level (pme-or-place template)
  (labels ((recurse (pme-or-place template)
             (let ((augmented-template-caller
                    (%augmented-template-caller #'recurse template)))
               (%analyze
                pme-or-place
                :on-place (lambda (place kind)
                            (values template place kind))
                :on-pme augmented-template-caller
                :on-ambiguous augmented-template-caller))))
    (recurse pme-or-place template)))

(defun %expand (place-modification-expression env)
  (let* ((oldp (and (typep place-modification-expression
                           '(cons (eql :old)))
                    (destructuring-bind (pme) (cdr place-modification-expression)
                      (prog1 t
                        (setf place-modification-expression pme)))))
         (finish
          (lambda (template place kind)
            (declare (ignore kind))
            (%finish template place env oldp))))
    (%analyze place-modification-expression
              :on-place
              (lambda (place kind)
                (error "Found the following (~(~A~)) place at top-level ~
                        instead of place-modification-expression:~%~S"
                       kind place))
              :on-pme
              (lambda (pme)
                (multiple-value-call finish
                  (%walk-conservatively-non-top-level pme)))
              :on-ambiguous
              (%make-speculative-walk-handler finish #'identity))))

(defmacro modify (&rest place-modification-expressions &environment env)
  `(progn
     ,@(map-bind (mapcar) ((expression place-modification-expressions))
         (%expand expression env))))
