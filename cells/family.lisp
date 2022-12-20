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

(eval-when (:compile-toplevel :execute :load-toplevel)
  (export '(model mdvalue family kids kid1 perishable)))

(defmodel model ()
  ((.mdvalue :cell t :initform nil :accessor mdvalue :initarg :mdvalue)))

(defmodel perishable ()
  ((expiration :cell t :initform nil :accessor expiration :initarg :expiration)))

(def-c-echo expiration ()
  (when new-value
    (not-to-be self)))

(defmodel family (model)
  ((.kids :cell t
         :initform (cv nil) ;; most useful
         :accessor kids
         :initarg :kids)
   (.kid-slots :cell t
         :initform nil
         :accessor kid-slots
         :initarg :kid-slots)))

(defmacro thekids (&rest kids)
  `(packed-flat! ,@(mapcar (lambda (kid)
                             (typecase kid
                               (keyword  `(make-instance ',(intern$ (symbol-name kid))))
                               (t `,kid)))
                     kids)))

(defmacro thekids2 (&rest kids)
  `(packed-flat! ,@(mapcar (lambda (kid)
                             (typecase kid
                               (keyword  `(make-instance ',(intern$ (symbol-name kid))))
                               (t `,kid)))
                           kids)))

(defun kid1 (self) (car (kids self)))

;; /// redundancy in following

(defmacro psib (&optional (selfform 'self))
  (let ((self (gensym)))
    `(bwhen (,self ,selfform)
        (find-prior ,self (kids (fmparent ,self))))))

(defmacro nsib (&optional (selfform 'self))
  (let ((self (gensym)))
    `(bwhen (,self ,selfform)
        (cadr (member ,self (kids (fmparent ,self)))))))

(defmacro ^priorSib (self)
   (let ((kid (gensym)))
      `(let* ((,kid ,self))
          (find-prior ,kid (^kids (fmParent ,kid))))))

(defmacro ^firstKidP (self)
   (let ((kid (gensym)))
      `(let ((,kid ,self))
          (eql ,kid (car (^kids (fmParent ,kid)))))))

(defmacro ^lastKidP (self)
   (let ((kid (gensym)))
      `(let ((,kid ,self))
          (null (cdr (member ,kid (^kids (fmParent ,kid))))))))

(defun md-adopt (fmparent self)
  (assert self)
  (assert fmparent)
  (assert (typep fmparent 'family))
  
  (trc nil "md-adopt >" :by fmparent)
  
  (let ((currparent (fmparent self))
        (selftype (type-of self)))
    (assert (or (null currparent)
                (eql fmparent currparent)))
    (unless (plusp (adopt-ct self))
      (incf (adopt-ct self))
      (setf (fmparent self) fmparent)

      (bwhen (kid-slots-fn (kid-slots (fmparent self)))
        (dolist (ksdef (funcall kid-slots-fn self) self)
          (let ((slot-name (ksname ksdef)))
            (trc self "got ksdef " slot-name)
            (when (md-slot-cell-type selftype slot-name)
              (trc self "got cell type " slot-name)
              (when (or (not (ksifmissing ksdef))
                      (and (null (c-slot-value self slot-name))
                        (null (md-slot-cell self slot-name))))
                (trc self "ks missing ok " slot-name)
                (multiple-value-bind (c-or-value suppressp)
                    (funcall (ksrule ksdef) self)
                  (unless suppressp
                    (trc self "c-install " slot-name c-or-value)
                    (c-install self slot-name c-or-value))))))))

      ; new for 12/02...
      (md-adopt-kids self)))
  self)

(defmethod md-adopt-kids (self) (declare (ignorable self)))
(defmethod md-adopt-kids ((self family))
  (when (slot-boundp self '.kids)
    (dolist (k (slot-value self '.kids))
      (unless (fmParent k)
        (md-adopt self k)))))




(defmethod c-slot-value ((self model-object) slot)
  (slot-value self slot))

(defun md-kids-change (self new-kids old-kids usage)
  (assert (listp new-kids))
  (assert (listp old-kids))
  (assert (not (member nil old-kids)))
  (assert (not (member nil new-kids)))

  (trc nil "md-kids-change > entry" usage new-kids old-kids)
  #+nah (when (and (trcp (car new-kids))
          (eql usage :md-slot-value-assume))
    (break "how here? ~a" self))

  (dolist (k old-kids)
     (unless (member k new-kids)
       (trc nil "kids change nailing lost kid" k)
       (not-to-be k)
       (setf (fmparent k) nil) ;; 020302kt unnecessary? anyway, after not-to-be since that might require fmparent
       ))

  (dolist (k new-kids)
      (unless (member k old-kids)       
        (if (eql :nascent (md-state k))
            (progn
              #+dfdbg (trc k "adopting par,k:" self k)
              (md-adopt self k))
          (unless (eql self (fmParent k))
            ;; 230126 recent changes to kids handling leads to dup kids-change calls
            (trc "feature not yet implemented: adopting previously adopted: parent, kid" self (type-of k))
            (trc "old" old-kids)
            (trc "new" new-kids)
            (break "bad state extant nkid ~a ~a ~a" usage k (md-state k))
            )))))

(def-c-echo .kids ((self family))
  (dolist (k new-value)
    (to-be k)))

(defun md-reinitialize (self)
  (unless (eql (md-state self) :nascent)
    (setf (md-state self) :nascent)
    (md-reinitialize-primary self)))

(defmethod md-reinitialize-primary :after ((self family))
  (dolist (kid (slot-value self '.kids)) ;; caused re-entrance to c? (kids self))
    (md-reinitialize kid)))

(defmethod md-reinitialize-primary (self)
  (cellbrk)
  (md-map-cells self nil (lambda (c)
                                 (setf (c-waking-state c) nil)
                                 (when (typep c 'c-ruled)
                                   (setf (c-state c) :unbound)))))

(defmethod kids ((other model-object))  nil)

(defmethod not-to-be :before ((fm family))
  (unless (md-untouchable fm)
    (trc nil "(not-to-be :before family) not closed stream, backdooropen; kids c-awake; kids c-state"
         *svuc-backdoor-open*
         (if (md-slot-cell fm '.kids)
           (c-waking-state (md-slot-cell fm '.kids))
           :no-kids-cell)
         (when (md-slot-cell fm '.kids)
           (c-state (md-slot-cell fm 'kids))))
    ;; use backdoor so if kids not yet ruled into
    ;; existence they won't be now just to not-to-be them
    (let ((svkids (slot-value fm '.kids)))
      (when (listp svkids)
	(dolist ( kid svkids)
	  (not-to-be kid)))))

  (trc nil "(not-to-be :before family) exit, kids state" (when (md-slot-cell fm 'kids)
                                                                (c-state (md-slot-cell fm 'kids)))))


;------------------  kid slotting ----------------------------
;
(cc-defstruct (kid-slotdef
           (:conc-name nil))
  ksname
  ksrule
  (ksifmissing t))

(defmacro mk-kid-slot ((ksname &key ifmissing) ksrule)
   `(make-kid-slotdef
     :ksname ',ksname
     :ksrule (lambda (self)
                 (declare (ignorable self))
                 ,ksrule)
     :ksifmissing ,ifmissing))

(defmacro def-kid-slots (&rest slot-defs)
  `(lambda (self)
     (declare (ignorable self))
     (list ,@slot-defs)))

(defmethod md-name (symbol)
     symbol)

(defmethod md-name ((nada null))
  (unless *stop*
    (setq *stop* t)
    (break "md-name called on nil")))