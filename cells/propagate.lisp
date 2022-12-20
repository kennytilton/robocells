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

(defparameter *echodone* nil)

(defun c-echo-defined (slot-name)
  (getf (symbol-plist slot-name) :echo-defined))

(defmethod (setf c-true-stalep) (newvalue (user c-ruled))
  #+dfdbg (trc user "setting c-true-stalep" user newvalue)
  (assert (find user (cells (c-model user)) :key #'cdr))
  (setf (cd-stale-p user) newvalue))

(defmethod (setf c-true-stalep) (newvalue (usersyn synapse))
  #+dfdbg (trc (syn-user usersyn) "synapse setting c-true-stalep" (syn-user usersyn) newvalue usersyn)
  (setf (cd-stale-p (syn-user usersyn)) newvalue))

(defmethod (setf c-true-stalep) (newvalue other)
  (declare (ignore other))
  newvalue)

(defun c-echo-initially (self slot-spec)
  "call during instance init. if echo is defined for slot, and value is non-nil (controversial) force initial echo."
  (trc *dbg* "c-echo-initially" self slot-spec
    (c-echo-defined (slot-spec-name slot-spec))
    (md-slot-cell self slot-spec))
  (if (c-echo-defined (slot-spec-name slot-spec))
    (bif (c (md-slot-cell self slot-spec))
         (etypecase c
           (c-variable (md-propagate self slot-spec (c-value c) nil nil))
           (c-ruled (md-slot-value self slot-spec))) ;; this will echo after calculating if not nil
         ;
         ; new for 22-03-07: echo even if slot value is nil...
         (c-echo-slot-name (slot-spec-name slot-spec)
                           self
                           (bd-slot-value self slot-spec)
                           nil nil))
    (bwhen (c (md-slot-cell self slot-spec))
      (c-ephemeral-reset c))))

#-(or cormanlisp clisp)
(defgeneric c-echo-slot-name (slotname self new old old-boundp) (:method-combination progn))


(defmethod c-echo-slot-name
    #-(or cormanlisp clisp) progn
  #+(or cormanlisp clisp) :before
  (slot-name self new old old-boundp)
  (declare (ignorable slot-name self new old old-boundp)))

#+(or cormanlisp clisp)
(defmethod c-echo-slot-name (slot-name self new old old-boundp)
  (declare (ignorable slot-name self new old old-boundp)))

;--------------- propagate  ----------------------------
;
; n.b. 990414kt the cell argument may have been optimized away,
; though it is still receiving final processing here.
;

(defun md-propagate (self slot-spec newvalue priorvalue priorvalue-supplied)
  (let (*c-calculators* 
        (*c-prop-depth*  (1+ *c-prop-depth*))
        (c (md-slot-cell self slot-spec)))
    ;
    ;------ debug stuff ---------
    ;
    (when *stop*
      (princ #\.)(princ #\!)
      (return-from md-propagate))
    
    (when c (trc nil "md-propagate> propping" self slot-spec (length (un-users c)) c))
    (when *c-debug*
      (when (> *c-prop-depth* 250)
        (trc "md-propagate deep" *c-prop-depth* self (slot-spec-name slot-spec) #+nah c))
      (when (> *c-prop-depth* 300)
          (break "md-propagate looping" c)
        ))
    
    (when c
      ; ------ flag dependents as stale ------------
      ; do before echo in case echo gets back to some user
      ;
      (dolist (user (un-users c))
        (trc nil  "md-prop now setting stale (changer, stale):" c user)
        (when (c-user-cares user)
          (setf (c-true-stalep user) c))))
    
    ; --- manifest new value as needed -----------
    (when (c-echo-defined (slot-spec-name slot-spec)) ;; /// faster than just dispatching?
      (when c (trc nil "md-prop now echoing" c))
      (c-echo-slot-name (slot-spec-name slot-spec)
        self
        newvalue
        priorvalue
        priorvalue-supplied)
      )
    
    (when c ; --- now propagate to dependents ------------
      (trc nil  "md-prop cehckeing dependents" c (un-users c))
      (let ((*cause* c))
        (dolist (user (un-users c))
          (trc nil "now rethinking (used, user):" c user)
          (when (c-user-cares user)
            (c-rethink user nil))))
      (c-ephemeral-reset c))
    ))

(defmethod c-user-cares (c) c) ;; ie, t
(defmethod c-user-cares ((s synapse))
  (syn-relevant s))

(defun c-ephemeral-reset (c)
    (when c
      (when (c-ephemeral-p c)
        (trc nil "c-ephemeral-reset resetting:" c)
        (setf (c-value c) nil)))) ;; good q: what does (setf <ephem> 'x) return? historically nil, but...?

;----------------- change detection ---------------------------------

(defun c-no-news (c newvalue oldvalue)
  ;;; (trc c "c-no-news > checking news between" newvalue oldvalue)
          
  (if (unst-delta-p c)
    (c-identity-p newvalue)
    (c-unchanged-p (c-model c) (c-slot-name c) newvalue oldvalue)))

(defmethod c-unchanged-p (self slotname newvalue oldvalue)
  (declare (ignore self slotname))
  (eql newvalue oldvalue))

(defmethod c-identity-p ((value null)) t)
(defmethod c-identity-p ((value number)) (zerop value))
(defmethod c-identity-p ((value cons))
  ;; this def a little suspect?
  (and (c-identity-p (car value))
       (c-identity-p (cdr value))))


;------------------- re think ---------------------------------

(defun c-rethink (c salvage)
  (when *stop*
    (princ #\.)
    (return-from c-rethink))
  ;;(trc "rethink entry: c, true-stale" c (c-true-stalep c))
  (unless (c-true-stalep c)
    (return-from c-rethink))

  (when *rethink-deferred*
    (trc nil "bingo!!!!!! rethink deferring" c *cause*)
    (push (list c salvage *cause*) *rethink-deferred*) ;; i think salvage is vestigial
    (return-from c-rethink))

  (bIf (interf (sw-detect-interference c nil))
    (progn
      (trc nil "!!!!!!!!!! rethink of " c :held-up-by interf)
      (c-pending-set c interf :interfered)
      #+dfdbg (when (trcp c)
                (trc "!!!!!!!!!! rethink of " c :held-up-by interf)
                #+nah (dump-stale-path interf)
                )
      (return-from c-rethink))
    (when (sw-pending c)
      (trc nil "no interference now for " c)
      (c-pending-set c nil :dis-interfered)))

  (typecase c
    (c-ruled 
     (if salvage
         (c-calculate-and-set c) ;; no handler; must succeed if exlicitly asked
       (handler-case ;; trap errors because....
           (progn
             (trc nil "c-rethink > calling calc-nset")
             (c-calculate-and-set c)) ;; might be a rethink which will succeed on susequent rethink
         (c-enabling (c)
                     (break "c-rethink > c-enabling error ~s on ~s" c c))
         #+rem (t (c) ;;leave remmed if possible to identify bugs; also, recode if possible to elim transient rethink failures
                  (inspect c)
                  (break "c-rethink > other error ~s on ~s" c c)))))
    
    (synapse
     (trc nil "c-rethink > testing rethink of: syn,salv,valu" c salvage (c-value (syn-used c)))
     (if (funcall (syn-fire-p c) c (c-value (syn-used c)))
         (progn
           (trc nil "c-rethink> decide yes on rethink on syn, valu" c (c-value (syn-used c)))
           (c-rethink (syn-user c) salvage))
       (trc nil "c-rethink> decide nooooo on rethink on synapse" c (syn-user c) salvage)))
    ))

(defmacro def-c-echo (slotname
                      (&optional (selfarg 'self) (newvarg 'new-value)
                                 (oldvarg 'old-value) (oldvargboundp 'old-value-boundp))
                      &body echobody)
  ;;;(trc "echo body" echobody)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',slotname :echo-defined) t))
     ,(if (eql (last1 echobody) :test)
          (let ((temp1 (gensym))
                (loc-self (gensym)))
            `(defmethod c-echo-slot-name #-(or clisp cormanlisp) progn ((slotname (eql ',slotname)) ,selfarg ,newvarg ,oldvarg ,oldvargboundp)
               (let ((,temp1 (bump-echo-count ,slotname))
                     (,loc-self ,(if (listp selfarg)
                                     (car selfarg)
                                   selfarg)))
                 (when (and ,oldvargboundp ,oldvarg)
                   (format t "~&echo ~d (~a ~a) old: ~a" ,temp1 ',slotname ,loc-self ,oldvarg))
                 (format t "~&echo ~d (~a ~a) new: ~a" ,temp1 ',slotname ,loc-self ,newvarg))))
        `(defmethod c-echo-slot-name
             #-(or clisp cormanlisp) progn
           ((slotname (eql ',slotname)) ,selfarg ,newvarg ,oldvarg ,oldvargboundp)
           (declare (ignorable ,(etypecase selfarg
                                  (list (car selfarg))
                                  (atom selfarg))
                               ,(etypecase newvarg
                                  (list (car newvarg))
                                  (atom newvarg))
                               ,(etypecase oldvarg
                                  (list (car oldvarg))
                                  (atom oldvarg))
                               ,(etypecase oldvargboundp
                                  (list (car oldvargboundp))
                                  (atom oldvargboundp))))
           ,@echobody))))

(defmacro bump-echo-count (slotname) ;; pure test func
  `(if (get ',slotname :echos)
       (incf (get ',slotname :echos))
     (setf (get ',slotname :echos) 1)))

