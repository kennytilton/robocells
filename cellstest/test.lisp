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


(eval-when (compile :execute load)
  (defmacro cv-assert (form &optional places (datum "~&~a~&...failed") &rest args)
  `(progn
     (assert  ,form ,places ,datum ,@(or args (list `',form)))
     (format t "~&ok: ~a~&" ',form)
     )))

(defun cv-test ()
  (let ((*c-debug* t))
    (cell-reset)
    (hello-world) ;; non-assertive
    (cv-test-engine)
    (cv-test-person)
;;;    ;; should fail: (df-test nil)
    (df-test t)
    (cv-test-family)
    (cv-family-values)
    (cv-kid-slotting)
    (boiler-1)
    (boiler-2)
    (boiler-3) ;; non-assertive
    (boiler-4) ;; non-assertive
    ))

(defun echo-clear (slot-name)
  (setf (getf (symbol-plist slot-name) 'echoed) nil)
  (setf (getf (symbol-plist slot-name) 'echo-new-value) :unbound)
  (setf (getf (symbol-plist slot-name) 'echo-old-value) :unbound)
  (setf (getf (symbol-plist slot-name) 'echo-old-boundp) nil))

(defun echoed (slot-name)
  (getf (symbol-plist slot-name) 'echoed))

(defun echo-new (slot-name)
  (bwhen (nv (getf (symbol-plist slot-name) 'echo-new-value))
    (unless (eql nv :unbound) nv)))

(defun echo-old (slot-name)
  (bwhen (nv (getf (symbol-plist slot-name) 'echo-old-value))
    (unless (eql nv :unbound) nv)))

(defun echo-old-boundp (slot-name)
  (getf (symbol-plist slot-name) 'echo-old-boundp))


(defmethod c-echo-slot-name
    #-(or cormanlisp clisp) progn
  #+(or cormanlisp clisp) :before
  (slot-name self new old old-boundp)
  (declare (ignorable slot-name self new old old-boundp))
  #-runtime-system
  (progn
    (trc nil "echo registering" slot-name new old old-boundp)
    (setf (getf (symbol-plist slot-name) 'echoed) t)
    (setf (getf (symbol-plist slot-name) 'echo-new-value) new)
    (setf (getf (symbol-plist slot-name) 'echo-old-value) old)
    (setf (getf (symbol-plist slot-name) 'echo-old-boundp) old-boundp)))