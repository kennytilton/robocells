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

(defvar *eex* 0)

(defmodel xx3 ()
  ((aa :cell t :initform (cv 0) :initarg :aa :accessor aa)
   (dd :cell t :initform (c? (min 0 (+ (^cc) (^bb)))) :initarg :dd :accessor dd)
   (ddx :cell t :initform (c? (+ (^cc) (^bb))) :initarg :ddx :accessor ddx)
   (cc :cell t :initform (c? (+ (^aa) (^bb))) :initarg :cc :reader cc)
   (bb :cell t :initform (c? (* 2 (^aa))) :initarg :bb :accessor bb)
   (ee :cell t :initform (c? (+ (^aa) (^dd))) :initarg :ee :reader ee)
   (eex :cell t :initform (c? (incf *eex*) (+ (^aa) (^ddx))) :initarg :eex :reader eex)
   ))

(def-c-echo aa ((self xx3))
    (trc nil "echo aa:" new-value))

(def-c-echo bb ((self xx3))
   (trc nil "echo bb:" new-value))

(def-c-echo cc ((self xx3))
    (trc nil "echo cc:" new-value))

(def-c-echo dd ((self xx3))
    (trc nil "echo dd:" new-value))

(def-c-echo ee ((self xx3))
   (trc nil "echo ee:" new-value))

(def-c-echo eex ((self xx3))
    (trc nil "echo eex:" new-value))

;;
;; here we look at just one problem, what i call dataflow interference. consider
;; a dependency graph underlying:
;;
;;     - a depends on b and c, and...
;;     - b depends on c
;;
;; if c changes, depending on the accident of the order in which a and b happened to
;; be first evaluated, a might appear before b on c's list of dependents (users). then the
;; following happens:
;;
;;     - c triggers a
;;     - a calculates off the new value of c and an obsolete cached value for b
;;     - a echos an invalid value and triggers any dependents, all of whom recalculate
;;         using a's invalid value
;;     - c triggers b
;;     - b recalculates and then triggers a, which then recalculates correctly and echos and triggers
;;         the rest of the df graph back into line
;;
;; the really bad news is that echos go outside the model: what if the invalid echo caused
;; a missile launch? sure, a subsequent correct calculation comes along shortly, but 
;; irrevocable damage may have been done.
;;
;; of historical interest: this flaw was corrected only recently. while it seems like a 
;; a serious flaw, it never caused a problem in practice. perhaps a year ago i do recall
;; applying a partial quick fix: in the above scenario, c flagged both a and b as "invalid"
;; before triggering a. that way, when a went to sample the un-refreshed b, b did a jit
;; recalculation and a came up with the correct value. so if the interference was just one
;; layer deep all was well.
;;
;; more historical amusement: that one-layer patch made it hard to concoct a set of interdependencies
;; to manifest intereference. that is why the example has more than just a few slots. the fix was also
;; dead simple, so i left it in for the first fix of
;; the deeper interference problems. but subsequently i found a problem arising from the
;; leftover original one-layer fix's interaction with the deeper fix, so i yanked the one-layer fix
;; and revised the deeper fix to cover everything. without the one-layer fix, this example
;; problem is overkill: it causes /double/ interference. but it has already proven it is a
;; tougher test, so i will stick with it on the chance that someday a change will be made which
;; a simpler test would not detect.
;;
;; the test run with (*df-interference-detection* t) succeeds and produces this output:
;;;
;;;0> echo aa: 2
;;;0> echo bb: 4
;;;0> echo cc: 6
;;;0> echo eex: 12
;;;0> echo ee: 2
;;;ok: (and (eql (aa it) 2) (eql (bb it) 4) (eql (cc it) 6)
;;;         (eql (dd it) 0) (eql (ddx it) 10) (eql (ee it) 2)
;;;         (eql (eex it) 12))
;;;ok: (eql *eex* 1)
;;;
;; change the first let to (*df-interference-detection* nil) and the test fails after producing this output:
;;;
;;;0> --------- 1 => (aa it) --------------------------
;;;0> echo aa: 1
;;;0> echo eex: 1
;;;0> echo ee: 1
;;;0> echo bb: 2
;;;0> echo eex: 3
;;;0> echo cc: 3
;;;0> echo eex: 6
;;;ok: (and (eql (aa it) 1) (eql (bb it) 2) (eql (cc it) 3))
;;;ok: (and (eql (dd it) 0) (eql (ddx it) 5))
;;;ok: (and (eql (ee it) 1) (eql (eex it) 6))
;;; error: (eql *eex* 1)...failed
;;
;;  because in fact the rule for eex ran not two but three times. notice that, as advertised, before
;; propagation completes all cells converge on the correct value--but in some cases they assume
;; illogical values and propagate them (most crucially via irretrievable echos) before getting to
;; the correct value.
;;

#+fail
(df-test nil)

#+succeed
(df-test t)

(defun df-test-t () (df-test t))

(defun df-test (dfid)
  (dotimes (x 1)
    (let* ((*df-interference-detection* dfid)
         (*eex* 0)
         (it (md-make 'xx3)))
    (cv-assert (eql *eex* 1))
    ;;(inspect it);;(cellbrk)
    (cv-assert (and (eql (aa it) 0)(eql (bb it) 0)(eql (cc it) 0)))
    (cv-assert (and (eql (dd it) 0)(eql (ddx it) 0)(eql (ee it) 0)(eql (eex it) 0)))
    
    ;;;- interference handling
    ;;;
    (let ((*eex* 0))
      (trc "--------- 1 => (aa it) --------------------------")
      (setf (aa it) 1)
      (cv-assert (and (eql (aa it) 1)(eql (bb it) 2)(eql (cc it) 3)))
      (cv-assert (and (eql (dd it) 0)(eql (ddx it) 5)))
      (cv-assert (and (eql (ee it) 1)(eql (eex it) 6)))
      (cv-assert (eql *eex* 1)))
    
    (let ((*eex* 0))
      (trc "--------- 2 => (aa it) --------------------------")
      (setf (aa it) 2)
      (cv-assert (and (eql (aa it) 2)(eql (bb it) 4)(eql (cc it) 6)
                      (eql (dd it) 0)(eql (ddx it) 10)(eql (ee it) 2)(eql (eex it) 12)))
      (cv-assert (eql *eex* 1)))
    )))


