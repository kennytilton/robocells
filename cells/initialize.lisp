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

(eval-when (compile eval load)
  (export '(c-envalue)))

(cc-defstruct (c-envaluer (:conc-name nil))
  envaluerule
  )

(defun c-awaken (c)
  (when *stop*
    (princ #\.)
    (return-from c-awaken))

  (assert (c-model c) () "c-awaken sees uninstalled cell" c)
  
  ; re-entry happen's normally
  ; nop it...
  ;
  (when (c-waking-state c)
    ;;(count-it :c-awaken :already)
    ;;(trc "c-awaken > already awake" c)
    (return-from c-awaken))

  ;;(trc "c-awaken > awakening" c)
  ;;(count-it :c-awaken)
  (setf (c-waking-state c) :awakening)
  (c-awaken-cell c)
  (setf (c-waking-state c) :awake)
  c)

(defun c-ephemeral-p (c)
  (eql :ephemeral (md-slot-cell-type (type-of (c-model c)) (c-slot-name c))))

(defmethod c-awaken-cell (c)
  (declare (ignorable c)))

(defmethod c-awaken-cell ((c c-variable))
  ;
  ; nothing to calculate, but every cellular slot should be echoed
  ;
  (let ((v (c-value c)))
    ;;(trc (c-model c) "c-awaken > calling echo" c v (slot-value (c-model c)(c-slot-name c)))
    (when (eql '.kids (c-slot-name c))
      (md-kids-change (c-model c) v nil :c-awaken-variable))
    (c-echo-slot-name (c-slot-name c) (c-model c) v nil nil)
    (c-ephemeral-reset c)))

(defmethod c-awaken-cell ((c c-ruled))
  ;
  ; ^svuc (with askers supplied) calls c-awaken, and now we call ^svuc crucially without askers
  ; this oddity comes from an incident in which an asker-free invocation of ^svuc
  ; successfully calculated when the call passing askers failed, i guess because askers not
  ; actually to be consulted given the algorithm still were detected as self-referential
  ; since the self-ref detector could not anticipate the algorithm's branching.
  ;
  (let (*c-calculators*)
    (c-calculate-and-set c)))

(defmethod c-awaken-cell ((c c-dependent))
  ;
  ; satisfy CormanCL bug
  ;
  (let (*c-calculators*)
    (c-calculate-and-set c)))

(defmethod c-awaken-cell ((c c-drifter))
  ;
  ; drifters *begin* valid, so the derived version's test for unbounditude
  ; would keep (drift) rule ever from being evaluated. correct solution
  ; (for another day) is to separate awakening (ie, linking to independent
  ; cs) from evaluation, tho also evaluating if necessary during
  ; awakening, because awakening's other role is to get an instance up to speed
  ; at once upon instantiation 
  ;
  (c-calculate-and-set c)
  (cond ((c-validp c) (c-value c))
        ((c-unboundp c) nil)
        (t "illegal state!!!")))
