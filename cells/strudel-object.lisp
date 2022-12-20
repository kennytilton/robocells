;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
;;;
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

;----------------- model-object ----------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
    (export '(strudel-object)))

(cc-defstruct (strudel-object (:conc-name nil))
  (q-state :nascent :type keyword) ; [nil | :nascent | :alive | :doomed]
  (q-name nil :type symbol)
  (q-parent nil)
  (q-cells nil :type list)
  (q-cells-flushed nil :type list)
  (q-adopt-ct 0 :type fixnum))

(defmethod strudel-initialize (self)
  (unless (q-name self)
      (setf (q-name self) (class-name (class-of self))))
      
  #+wait (when (q-parent self)
    (q-adopt (q-parent self) self))
  self)

(defmethod cells ((self strudel-object))
  (q-cells self))

(defmethod (setf cells) (new-value (self strudel-object))
  (setf (q-cells self) new-value))

(defmethod kids ((other strudel-object))  nil)

(defun q-install (self sn c)
  (assert (typep c 'cell))
  (trc nil "installing cell" sn c)
  (setf
   (c-model c) self
   (c-slot-spec c) sn
   (md-slot-cell self sn) c))

(defmethod (setf md-state) (newv (self strudel-object))
  (setf (q-state self) newv))

(defmethod md-state ((self strudel-object))
  (q-state self))

(defmethod md-name ((self strudel-object)) (q-name self))
(defmethod fmparent ((self strudel-object)) (q-parent self))

(defmethod print-object ((self strudel-object) s)
  (format s "~a" (or (md-name self) (type-of self))))

(defun q-slot-value (slot-c)
  (when *stop*
    (princ #\.)
    (return-from q-slot-value))
  ;; (count-it :q-slot-value slot-name slot-spec))

;;;  (when (eql :nascent (q-state self))
;;;    (md-awaken self))

  (let ((slot-value (typecase slot-c
                      (c-variable (c-value slot-c))

                      (c-ruled (cond
                                ((c-validp slot-c) (c-value slot-c)) ;; good to go
                                    
                                ((find slot-c *c-calculators*) ;; circularity
                                 (setf *stop* t)
                                 (trc "q-slot-value breaking on circlularity" slot-c *c-calculators*)
                                 (error "cell ~a midst askers: ~a" slot-c *c-calculators*))
                                    
                                (t (let ((*cause* :on-demand)) ; normal path first time asked
                                     (trc nil  "md-slot-value calc" self slot-spec *c-calculators*)
                                     (c-calculate-and-set slot-c)))))
                      (otherwise (return-from q-slot-value slot-c)))))
    
      (bif (synapse (when (car *c-calculators*)
                      (c-link-ex slot-c)))
        (c-relay-value synapse slot-value)
        slot-value)))




(defmethod md-awaken :around ((self strudel-object))
  (trc nil "md-awaken entry" self (md-state self))
  (assert (eql :nascent (md-state self)))
  ;; (trc nil "awaken doing")
  (count-it :md-awaken)
  ;;(count-it 'mdawaken (type-of self))
  (setf (md-state self) :awakening)
  ;; (trc "md-awaken entry" self)
  (dolist (esd (class-slots (class-of self)))
    ;;(trc "md-awaken scoping slot" self (slot-definition-name esd))
    (when (md-slot-cell-type (type-of self) (slot-definition-name esd))
      (let ((slot-name (slot-definition-name esd)))
        (if (not (c-echo-defined slot-name))
            (progn ;; (count-it :md-awaken :no-echo-slot slot-name)
              (trc nil "md-awaken deferring cell-awaken since no echo" self esd))
          
          (let ((cell (md-slot-cell self slot-name)))
            (trc nil "md-awaken finds md-esd-cell " self slot-name cell)
            
            
            (if cell
                (c-awaken cell)
              ;
              ; next bit revised to avoid double-echo of optimized cells
              ;
              (progn
                (when (eql '.kids slot-name)
                  (bwhen (sv (slot-value self '.kids))
                    (md-kids-change self sv nil :md-awaken-slot)))
                (c-echo-initially self slot-name)))))))
    )
  
  (setf (md-state self) :awake)
  self)

(defmethod md-slot-value-store ((self strudel-object) slot-spec new-value)
  (declare (ignorable slot-spec))
  new-value)