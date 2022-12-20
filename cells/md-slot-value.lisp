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

(defun md-slot-cell-flushed (self slot-spec)
  (assocv (slot-spec-name slot-spec) (cells-flushed self)))

(defun md-slot-value (self slot-spec &aux (slot-c (md-slot-cell self slot-spec)))
  (when *stop*
    (princ #\.)
    (return-from md-slot-value))
  ;; (count-it :md-slot-value (slot-spec-name slot-spec))

  (when (eql :nascent (md-state self))
    (md-awaken self))

  (let ((slot-value (etypecase slot-c
                      (null (bd-slot-value self slot-spec))

                      (c-variable (c-value slot-c))

                      (c-ruled (cond
                                ((c-validp slot-c) (c-value slot-c)) ;; good to go
                                    
                                ((find slot-c *c-calculators*) ;; circularity
                                 (setf *stop* t)
                                 (trc "md-slot-value breaking on circlularity" slot-c *c-calculators*)
                                 (break ;; problem when testing cells on some CLs
                                  "cell ~a midst askers: ~a" slot-c *c-calculators*))
                                    
                                (t (let ((*cause* :on-demand)) ; normal path first time asked
                                     (trc nil  "md-slot-value calc" self slot-spec *c-calculators*)
                                     (c-calculate-and-set slot-c))))))))
    
    (bif (synapse (when (car *c-calculators*)
                    (c-link-ex slot-c)))
         (c-relay-value synapse slot-value)
         slot-value)))

;-------------------------------------------------------------

(defun (setf md-slot-value) (newvalue self slot-spec)
  (let ((c (md-slot-cell self slot-spec)))

    (when *c-debug*
      (c-setting-debug self slot-spec c newvalue))

    (unless c
      (cellstop)
      (error "(setf md-slot-value)> cellular slot ~a of ~a cannot be setf unless initialized to c-variable"
        slot-spec self)
      )

    (if (unst-setting-p c)
      (if (unst-cyclic-p c)
          newvalue
          (error "setf of ~a propagated back; declare as cyclic (cv8...)" c))
      (let ((absorbedvalue (c-absorb-value c newvalue)))
        (with-dataflow-management (c)
          (md-slot-value-assume self slot-spec absorbedvalue)) ;; /// uh-oh. calc-n-set uses this return value
        absorbedvalue))))

;;;(defmethod trcp ((c c-ruled))
;;;  ;;(trc "trcp ruled" (c-slot-name c) (md-name (c-model c)))
;;;  (and (eql 'clo::px (c-slot-name c))
;;;    (eql :mklabel (md-name (c-model c)))))


(defun md-slot-value-assume (self slot-spec absorbedvalue
                                  &aux
                                  (c (md-slot-cell self slot-spec))
                                  (priorstate (when c (c-state c)))
                                  (priorvalue (when c (c-value c)))
                                  )
  (md-slot-value-store self (slot-spec-name slot-spec)
    (if c
        (setf (c-value c) absorbedvalue)
      absorbedvalue))
  
  (when (typep c 'c-ruled)
    (trc nil " setting cellstate :valid" c)
    (setf (c-state c) :valid)
    (setf (cd-stale-p c) nil)
    (setf (c-waking-state c) :awake)
    (c-pending-set c nil :sv-assume)
    (c-optimize-away?! c)) ;;; put optimize as early as possible
  
  ;--- propagation -----------
  ;
  (unwind-protect
      (if (and (eql priorstate :valid) ;; ie, priorvalue meaningful (nil is ambiguous)
               (c-no-news c absorbedvalue priorvalue))
          (progn
            (trc nil "(setf md-slot-value) >no-news" priorstate (c-no-news c absorbedvalue priorvalue))
            #+not (count-it :no-news))
        (progn
          (when (eql '.kids (slot-spec-name slot-spec))
            #+dfdbg (dolist (K absorbedvalue) (trc k "md-slot-value-assume -> kids change" k self))
            (md-kids-change self absorbedvalue priorvalue :md-slot-value-assume))
          (md-propagate self slot-spec absorbedvalue priorvalue (not (eql :unbound priorstate)))))
    (when c
      (setf (unst-setting-p c) nil)))
  absorbedvalue)

