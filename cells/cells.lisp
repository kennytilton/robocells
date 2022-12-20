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

(defpackage :cells
    (:use "COMMON-LISP"
      #+allegro "EXCL"
      #-(or cormanlisp cmu) "CLOS"
      #+mcl "CCL"
      )
  #+clisp (:import-from #:clos "CLASS-SLOTS" "CLASS-PRECEDENCE-LIST")
  #+cmu (:import-from "PCL" "CLASS-PRECEDENCE-LIST" "CLASS-SLOTS"
		      "SLOT-DEFINITION-NAME")
  (:export "CELL" "CV" "C?" "C??" "WITHOUT-C-DEPENDENCY" "SELF" "*SYNAPSE-FACTORY*"
    ".CACHE." "C-CACHE" ".CAUSE"
    "DEFMODEL" "CELLBRK" "C-AWAKEN" "DEF-C-ECHO" "NEW-VALUE" "OLD-VALUE" "C..."
    "MKPART" "THEKIDS" "NSIB" "MDVALUE" "^MDVALUE" ".MDVALUE" "KIDS" "^KIDS" ".KIDS"
    "CELL-RESET" "UPPER" "FM-MAX" "NEAREST" "^FM-MIN-KID" "^FM-MAX-KID" "MK-KID-SLOT" 
    "DEF-KID-SLOTS" "FIND-PRIOR" "FM-POS" "KIDNO" "FM-INCLUDES" "FM-ASCENDANT-COMMON" 
    "FM-KID-CONTAINING" "FM-FIND-IF" "FM-ASCENDANT-IF" "C-ABS" "FM-COLLECT-IF" "CV8" "PSIB"
    "TO-BE" "NOT-TO-BE" "SSIBNO" "MD-AWAKEN"
    #:*trcdepth* #:*dbg* #:trc #:trcp #:wnotrc #:wtrc #:eko "EK"
    #:bwhen #:bif #:maptimes #:dowhile #:x-or #-allegro #:while #-allegro #:until 
    #:intern$ #:assocv #:assocv-setf #:*stop* #:with-dynamic-fn #:packed-flat!
    #:max-if #:min-if #:class-slot-named #:shortc #:longc #:make$ #:conc$
    #:with-counts #:count-it #:timex #:with-metrics #:profilex
    #+mcl #:class-slots
    #-mcl #:true #:false
    "XOR"

    )
  #+allegro (:shadowing-import-from #:excl #:fasl-write #:fasl-read #:gc)
  )

(in-package :cells)

(defconstant *c-optimizep* t)
(defvar *c-prop-depth* 0)
(defvar *cause* nil)
(defvar *rethink-deferred* nil)
(defvar *synapse-factory* nil)
(defvar *sw-looping* nil)
(defparameter *to-be-awakened* nil)
(defvar *trcdepth* 0)

(defparameter *c-debug*
  #+runtime-system nil
  #-runtime-system t)

(defvar *stop* nil)

(defun stop ()
  (setf *stop* t))

(defvar *c-calculators* nil)

(defmacro ssibno () `(position self (^kids .parent.)))

(defmacro gpar ()
  `(fm-grandparent self))

(defmacro nearest (selfform type)
   (let ((self (gensym)))
   `(bwhen (,self ,selfform)
       (if (typep ,self ',type) ,self (upper ,self ,type)))))

(defmacro def-c-trace (model-type &optional slot cell-type)
  `(defmethod trcp ((self ,(case cell-type
                             (:c? 'c-dependent)
                             (otherwise 'cell))))
     (and (typep (c-model self) ',model-type)
       ,(if slot
            `(eq (c-slot-name self) ',slot)
          `t))))

(defmacro with-dataflow-management ((c-originating) &body body)
     (let ((fn (gensym)))
        `(let ((,fn (lambda () ,@body)))
           (declare (dynamic-extent ,fn))
           (call-with-dataflow-management ,c-originating ,fn))))

(defmacro without-c-dependency (&body body)
  `(let (*c-calculators*) ,@body))

(defmacro without-propagating ((slotname objxpr) &body body)
  (let ((c (gensym))
        (c-delta (gensym)))
    `(let ((,c (slot-value ,objxpr ',slotname)))
       (push (cons ,c nil) *c-noprop*)
       (progn ,@body)
       (let ((,c-delta (assoc ,c *c-noprop*)))
         (assert ,c-delta)
         (setf *c-noprop* (delete ,c-delta *c-noprop*))
         (when (cdr ,c-delta) ;; if changed, will be set to /list/ containing priorvalue
           (,c (cadr ,c-delta) (caddr ,c-delta)))))))

