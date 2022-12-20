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

(defstruct (qrock (:include strudel-object)(:conc-name nil))
  (.accel 32)
  (.elapsed (cv 0))
  (.dist (c? (floor (* (qaccel self)(expt (elapsed self) 2)) 2))))

(defun qaccel (self)
  (q-slot-value (.accel self)))

(defun (setf qaccel) (newvalue self)
  (setf (md-slot-value self '.accel) newvalue))

(defun elapsed (self)
  (q-slot-value (.elapsed self)))

(defun (setf elapsed) (newvalue self)
  (setf (md-slot-value self '.elapsed) newvalue))

(defun dist (self)
  (q-slot-value (.dist self)))

(defun (setf dist) (newvalue self)
  (setf (md-slot-value self '.dist) newvalue))

(def-c-echo .accel () (trc ".accel" self new-value old-value))
(def-c-echo .elapsed ()
  (when (typep new-value 'cell) (break))
  (trc ".elapsed" self new-value old-value))
(def-c-echo .dist () (trc ".dist" self new-value old-value))

(progn
  (setf (md-slot-cell-type 'qrock '.accel) t)
  (setf (md-slot-cell-type 'qrock '.elapsed) t)
  (setf (md-slot-cell-type 'qrock '.dist) t))

(defun make-cell-qrock (&rest iargs)
  (let ((self (apply #'make-qrock iargs)))
    (strudel-initialize self)
    (trc "qcs" (q-cells self))
    self))

#+test
(let (*to-be-awakened*)
  (let ((r (to-be (make-cell-qrock))))
    (dotimes (n 5)
      (trc "--------------- time " n)
      (setf (elapsed r) n))))

(defmethod strudel-initialize :around ((self qrock))
  (flet ((ci (sn sv)
           (when (typep sv 'cell)
             (q-install self sn sv))))
    (ci '.accel (.accel self))
    (ci '.elapsed (.elapsed self))
    (ci '.dist (.dist self)))
  (call-next-method))


