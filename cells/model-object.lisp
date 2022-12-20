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

;----------------- model-object ----------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
    (export '(md-name mdwhen fmparent .parent.)))

(defclass model-object ()
  ((.md-state :initform nil :accessor md-state) ; [nil | :nascent | :alive | :doomed]
   (.md-name :initform nil :initarg :md-name :accessor md-name)
   (.mdwhen :initform nil :initarg :mdwhen :accessor mdwhen)
   (.fmparent :initform nil :initarg :fmparent :accessor fmparent)
   (.cells :initform nil :initarg :cells :accessor cells)
   (.cells-flushed :initform nil :initarg cells :accessor cells-flushed
                   :documentation "cells supplied but un-whenned or optimized-away")
   (adopt-ct :initform 0 :accessor adopt-ct)))

(defmethod print-object ((self model-object) s)
  (format s "~a" (or (md-name self) (type-of self))))

(define-symbol-macro .parent. (fmparent self))

(defun md-cell-defs (self)
  (get (type-of self) :cell-defs))

(defmethod md-slot-cell (self slot-spec)
  (assocv (slot-spec-name slot-spec) (cells self)))

(defun md-slot-cell-type (class-name slot-spec)
  (bif (entry (assoc (slot-spec-name slot-spec) (get class-name :cell-defs)))
    (cdr entry)
    (dolist (super (class-precedence-list (find-class class-name)))
      (bIf (entry (assoc (slot-spec-name slot-spec) (get (c-class-name super) :cell-defs)))
	   (return (cdr entry))))))
       

(defun (setf md-slot-cell-type) (new-type class-name slot-spec)
  (assocv-setf (get class-name :cell-defs) (slot-spec-name slot-spec) new-type))

(defmethod md-slot-value-store ((self model-object) slot-spec new-value)
  (setf (slot-value self (slot-spec-name slot-spec)) new-value))

;----------------- navigation: slot <> initarg <> esd <> cell -----------------

#+cmu
(defmethod c-class-name ((class pcl::standard-class))
  (pcl::class-name class))

(defmethod c-class-name (other) (declare (ignore other)) nil)

(defmethod c-class-name ((class standard-class))
  (class-name class))

(defmethod cellwhen (other) (declare (ignorable other)) nil)

(defun (setf md-slot-cell) (newcell self slot-spec)
  (bif (entry (assoc (slot-spec-name slot-spec) (cells self)))
    (let ((old (cdr entry))) ;; s/b being supplanted by kid-slotter
      (assert (null (un-users old)))
      (assert (null (cd-useds old)))
      (trc nil "replacing in model .cells" old newcell self)
      (rplacd entry newcell))
    (progn
      (trc nil "adding to model .cells" newcell self)
      (push (cons (slot-spec-name slot-spec) newcell)
        (cells self)))))

(defun md-map-cells (self type celldo)
  (map type (lambda (cellentry)
                (bwhen (cell (cdr cellentry))
                       (unless (listp cell)
                         (funcall celldo cell))))
        (cells self)))

(defun c-install (self sn c)
  (assert (typep c 'cell))
  (trc nil "installing cell" sn c)
  (setf
   (c-model c) self
   (c-slot-spec c) sn
   (md-slot-cell self sn) c
   (slot-value self sn) (when (typep c 'c-variable)
                          (c-value c))))

;------------------ md obj initialization ------------------

(defmethod shared-initialize :after ((self model-object) slotnames
                                      &rest initargs &key fmparent mdwhen
                                      &allow-other-keys)
  (declare (ignorable initargs slotnames fmparent mdwhen))

  (dolist (esd (class-slots (class-of self)))
    (let* ((sn (slot-definition-name esd))
           (sv (when (slot-boundp self sn)
                 (slot-value self sn))))
      (when (typep sv 'cell)
        (if (md-slot-cell-type (type-of self) sn)
            (c-install self sn sv)
          (when *c-debug*
            (trc "cell ~a offered for non-cellular model/slot ~a/~a" sv self sn))))))

  (md-initialize self))

;;;(defun pick-if-when-slot (esd mdwhen &aux (cellwhen (cellwhen esd)))
;;;  (or (null cellwhen)
;;;      (some-x-is-in-y cellwhen mdwhen)))

(defmethod md-initialize (self)
  (when (slot-boundp self '.md-name)
    (unless (md-name self)
      (setf (md-name self) (c-class-name (class-of self)))))
      
  (when (fmparent self)
    (md-adopt (fmparent self) self))

  (setf (md-state self) :nascent))

(defun cells-clear (self)
  "allow gc"
  ;;
  ;; too extreme? 'close-device went after slot when a class 
  ;; ended up without cells--should not be a crime 2k0320kt
  ;; (slot-makunbound self '.cells)
  ;; ...
  (setf (cells self) nil) ;; try instead
  )


;--------- awaken only when ready (in family, for models) --------


(defmethod md-awaken ((self model-object))
  ;; (trc nil "md-awaken entry" self (md-state self))
  (assert (eql :nascent (md-state self)))
  ;; (trc nil "awaken doing")
  (count-it :md-awaken)
  ;;(count-it 'mdawaken (type-of self))
  (setf (md-state self) :awakening)
  ;; (trc "md-awaken entry" self)
  (dolist (esd (class-slots (class-of self)))
    ;;(trc nil "md-awaken scoping slot" self (slot-definition-name esd))
    (when (md-slot-cell-type (type-of self) (slot-definition-name esd))
      (let ((slot-name (slot-definition-name esd)))
        (if (not (c-echo-defined slot-name))
            (progn ;; (count-it :md-awaken :no-echo-slot slot-name)
              (trc nil "md-awaken deferring cell-awaken since no echo" self esd))
          
          (let ((cell (md-slot-cell self slot-name)))
            (trc nil "md-awaken finds md-esd-cell " cell)
            (when *c-debug*
              ;
              ; check to see if cell snuck into actual slot value...
              ;
              (bwhen (sv (slot-value self slot-name))
                (when (typep sv 'cell)
                  (error "md-awaken ~a found cell ~a in slot ~a" self sv esd))))
            
            (if cell
		(c-awaken cell)
	       (progn ;; next bit revised to avoid double-echo of optimized cells
		 (when (eql '.kids slot-name)
		   (bwhen (sv (slot-value self '.kids))
		      (md-kids-change self sv nil :md-awaken-slot)))
		 (c-echo-initially self slot-name))))))))
  
  (setf (md-state self) :awake)
  self)
  
