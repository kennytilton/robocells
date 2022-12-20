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

(defun c-setting-debug (self slot-spec c newvalue)
  (declare (ignorable newvalue))
  (if (null c)
      (progn
        (format t "c-setting-debug > constant  ~a in ~a may not be altered..init to (cv nil)"
              slot-spec self)
        (error "setting-const-cell"))
    (let ((self (c-model c))
          (slot-spec (c-slot-spec c)))
      ;(trc "c-setting-debug sees" c newvalue self slot-spec)
      (when (and c (not (and slot-spec self)))
        ;; cv-test handles errors, so don't set *stop* (cellstop)
        (error 'c-unadopted :cell c))
      (typecase c
        (c-variable)
        (c-independent)
        (c-dependent
         ;(trc "setting c-dependent" c newvalue)
         (format t "c-setting-debug > ruled  ~a in ~a may not be setf'ed"
           (c-slot-name c) self)
         (error "setting-ruled-cell"))
        ))))

(defun c-absorb-value (c value)
  (typecase c
    (c-drifter-absolute (c-value-incf c value 0)) ;; strange but true
    (c-drifter (c-value-incf c (c-value c) value))
    (t value)))

(defmethod c-value-incf (c (envaluer c-envaluer) delta)
  (assert (c-model c))
  (c-value-incf c (funcall (envaluerule envaluer) (c-model c))
                 delta))

(defmethod c-value-incf (c (base number) delta)
  (declare (ignore c))
  (if delta
    (+ base delta)
    base))


;----------------------------------------------------------------------

(defun bd-slot-value (self slot-spec)
  (slot-value self (slot-spec-name slot-spec)))

(defun (setf bd-slot-value) (newvalue self slot-spec)
  (setf (slot-value self (slot-spec-name slot-spec)) newvalue))

(defun bd-bound-slot-value (self slot-spec callerid)
  (declare (ignorable callerid))
  (when (bd-slot-boundp self (slot-spec-name slot-spec))
    (bd-slot-value self (slot-spec-name slot-spec))))

(defun bd-slot-boundp (self slot-spec)
  (slot-boundp self (slot-spec-name slot-spec)))

(defun bd-slot-makunbound (self slot-spec)
  (slot-makunbound self (slot-spec-name slot-spec)))

#| sample incf
(defmethod c-value-incf ((base fpoint) delta)
  (declare (ignore model))
  (if delta
    (fp-add base delta)
    base))
|#
