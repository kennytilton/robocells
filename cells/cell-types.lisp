;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
;;;
;;; Copyright © 1995,2003 by Kenneth William Tilton.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

(in-package :cells)

(defun slot-spec-name (slot-spec)
  slot-spec)

(cc-defstruct (cell (:conc-name c-))
  waking-state
  model
  slot-spec
  value
  )

(defun c-slot-name (c)
  (slot-spec-name (c-slot-spec c)))

(defun c-validate (self c)
  (when (not (and (c-slot-spec c) (c-model c)))
;;;      (setf *stop* t)
      (format t "~&unadopted cell: ~s md:~s, awake:~s" c self (c-waking-state self))
      (error 'c-unadopted :cell c)))

(defmethod c-when (other)
	(declare (ignorable other)) nil) ;; /// needs work

(cc-defstruct (synapse
            (:include cell)
            (:conc-name syn-))
  user
  used
  (relevant t) ;; not if unused during subsequent eval. but keep to preserve likely state
  fire-p
  relay-value)

(defmacro mksynapse ((&rest closeovervars) &key trcp fire-p relay-value)
  (let ((used (copy-symbol 'used)) (user (copy-symbol 'user)))
    `(lambda (,used ,user)
       ,(when trcp
          `(trc "making synapse between user" ',trcp ,user :and :used ,used))
       (let (,@closeovervars)
         (make-synapse
          :used ,used
          ;;; 210207kt why? use (c-model (syn-used <syn>)) :c-model (c-model ,used)
          :user ,user
          :fire-p ,fire-p
          :relay-value ,relay-value)))))

(defmethod print-object ((syn synapse) stream)
  (format stream "{syn ~s ==> ~s" (syn-used syn) (syn-user syn)))


(defmethod c-true-stalep ((syn synapse))
  (cd-stale-p (syn-user syn)))

(cc-defstruct (c-user-notifying
            (:include cell)
            (:conc-name un-))
  (users nil :type list))

(cc-defstruct (c-unsteady
            (:include c-user-notifying)
            (:conc-name unst-))
  cyclic-p
  delta-p
  setting-p)

(cc-defstruct (c-variable
            (:include c-unsteady)))

(cc-defstruct (c-ruled
            (:include c-unsteady)
            (:conc-name cr-))
  (state :unbound :type symbol)
  (rethinking 0 :type number)
  rule)

(defun c-optimized-away-p (c)
  (eql :optimized-away (c-state c)))

;----------------------------


(defmethod c-true-stalep (c)
  (declare (ignore c)))

(cc-defstruct (c-independent
            ;;
            ;; these do not optimize away, because also these can be set after initial evaluation of the rule,
            ;; so users better stay tuned.
            ;; the whole idea here is that the old idea of having cv bodies evaluated immediately finally
            ;; broke down when we wanted to say :kids (cv (list (fm-other vertex)))
            ;;
            (:include c-ruled)))

;;;(defmethod trcp ((c c-dependent))
;;;  (trcp (c-model c)))

(defconstant *cd-usagect* 32)

(cc-defstruct (c-dependent
            (:include c-ruled)
            (:conc-name cd-))
  (useds nil :type list)
  (code nil :type list) ;; /// feature this out on production build
  (usage (make-array *cd-usagect* :element-type 'bit
                        :initial-element 0) :type vector)
  stale-p
  )


(defmethod c-true-stalep ((c c-dependent))
  (cd-stale-p c))

(cc-defstruct (c-stream
            (:include c-ruled)
            (:conc-name cs-))
  values)

;;; (defmacro cell~ (&body body)
;;;   `(make-c-stream
;;;     :rule (lambda ,@*c-lambda*
;;;                 ,@body)))

(cc-defstruct (c-drifter
            (:include c-dependent)))

(cc-defstruct (c-drifter-absolute
            (:include c-drifter)))

;_____________________ accessors __________________________________


(defun (setf c-state) (new-value c)
  (if (typep c 'c-ruled)
      (setf (cr-state c) new-value)
    new-value))

(defun c-state (c)
  (if (typep c 'c-ruled)
      (cr-state c)
    :valid))

(defun c-unboundp (c)
  (eql :unbound (c-state c)))

(defun c-validp (c)
  (find (c-state c) '(:valid :optimized-away)))

;_____________________ print __________________________________

(defmethod print-object :before ((c c-variable) stream)
  (declare (ignorable c))
  (format stream "[var:"))

(defmethod print-object :before ((c c-dependent) stream)
	(declare (ignorable c))
  (format stream "[dep~a:" (cond
                            ((null (c-model c)) #\0)
                            ((eq :eternal-rest (md-state (c-model c))) #\_)
                            ((cd-stale-p c) #\#)
                            ((sw-pending c) #\?)
                            (t #\space))))

(defmethod print-object :before ((c c-independent) stream)
	(declare (ignorable c))
  (format stream "[ind:"))

(defmethod print-object ((c cell) stream)
  (c-print-value c stream)
  (format stream "=~a/~a]"
    (symbol-name (or (c-slot-name c) :anoncell))
    (or (c-model c) :anonmd))
  #+dfdbg (unless *stop*
    (assert (find c (cells (c-model c)) :key #'cdr)))
  )

;__________________

(defmethod c-print-value ((c c-ruled) stream)
  (format stream "~a" (cond ((unst-setting-p c) "<^^^>")
                            ((c-validp c) "<vld>")
                            ((c-unboundp c) "<unb>")
                            ((cd-stale-p c) "<obs>")
                            (t "<err>"))))

(defmethod c-print-value (c stream)
  (declare (ignore c stream)))

;____________________ constructors _______________________________

(defmacro c? (&body body)
  `(make-c-dependent
    :code ',body
    :rule (lambda (c &aux (self (c-model c)))
              (declare (ignorable self c))
              ,@body)))

(defmacro c-cache () `(c-value c))

(define-symbol-macro .cache. (c-value c))

(defmacro c?? ((&key (tagp nil) (in nil) (trigger nil) (out t))&body body)
  (let ((result (copy-symbol 'result))
        (thetag (gensym)))
     `(make-c-dependent
       :code ',body
       :rule (lambda (c &aux (self (c-model c)))
                 (declare (ignorable self c))
                 (let ((,thetag (gensym "tag"))
                       (*trcdepth* (1+ *trcdepth*))
                       )
                   (declare (ignorable self ,thetag))
                   ,(when in
                       `(trc "c??> entry" (c-slot-name c) (c-model c) (when ,tagp ,thetag)))
                   ,(when trigger `(trc "c??> trigger" *cause* c))
                   (count-it :c?? (c-slot-name c) (md-name (c-model c)))
                   (let ((,result (progn ,@body)))
                     ,(when out `(trc "c?? result:" ,result (c-slot-name c) (when ,tagp ,thetag)))
                     ,result))))))

(defmacro c2?? (&body body)
  (let ((result (copy-symbol 'result)))
     `(make-c-dependent
       :rule (lambda (cell askingcells)
                   (declare (ignorable cell askingcells))
                   (trc "c2??> entry" c)
                   (let ((,result (progn ,@body)))
                      (trc "c??> returning" ,result)
                      ,result)))))

(defmacro c2? (&body body)
  `(make-c-dependent
    :rule (lambda (cell askingcells)
                (declare (ignorable cell askingcells))
                ,@body)))

(defmacro cv (defn)
  `(make-c-variable
    :value ,defn)) ;; use c-independent if need deferred execution

(defmacro cv8 (defn)
  `(make-c-variable
    :cyclic-p t
    :value ,defn)) ;; use c-independent if need deferred execution

(defmacro c... ((value) &body body)
  `(make-c-drifter
    :code ',body
    :value ,value
    :rule (lambda (c &aux (self (c-model c)))
              (declare (ignorable self c))
              ,@body)))

(defmacro c-abs (value &body body)
  `(make-c-drifter-absolute
    :code ',body
    :value ,value
    :rule (lambda (c &aux (self (c-model c)))
                (declare (ignorable self c))
              ,@body)))


(defmacro c-envalue (&body body)
  `(make-c-envaluer
    :envaluerule (lambda (self)
                     (declare (ignorable self))
                     ,@body)))


(defmacro c8 (&body body)
  `(make-c-dependent
    :cyclic-p t
    :rule (lambda (c)
                (let ((self (c-model c))
                      (*c-calculators* (cons c *c-calculators*))
                      *synapse-factory* ;; clear then re-estab via with-synapse on specific dependencies
                      )
                  ,@body))))
