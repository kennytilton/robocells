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

(defmodel engine ()
  ((fuel :initarg :fuel :initform nil :accessor fuel)
   (cylinders :cell t :initarg :cylinders :initform (cv 4) :accessor cylinders)
   (valves-per-cylinder :cell t :initarg :valves-per-cylinder :initform 2 :accessor valves-per-cylinder)
   (valves :cell t
           :initarg :valves
           :accessor valves
           :initform (c? (* (valves-per-cylinder self)
                            (cylinders self))))
   (mod3 :cell t
         :initarg :mod3 :initform nil :accessor mod3)
   (mod3ek :cell t
         :initarg :mod3ek :initform nil :accessor mod3ek)
   ))

(defmethod c-unchanged-p ((self engine) (slotname (eql 'mod3)) new-value old-value)
  (flet ((test (it) (zerop (mod it 3))))
    (eql (test new-value) (test old-value))))

(def-c-echo mod3ek () (trc "mod3ek echo" self))

(defmethod c-unchanged-p ((self engine) (slotname (eql 'mod3ek)) new-value old-value)
  (flet ((test (it) (zerop (mod it 3))))
    (eql (test new-value) (test old-value))))

(def-c-echo cylinders () 
  ;;(when *dbg* (break))
  (trc "cylinders echo" self old-value new-value))

(defvar *propagations* nil)

(defmodel engine-w-initform ()
  ((cylinders :cell t :initform 33 :reader cylinders)))

(defclass non-model ()())
(defmodel faux-model (non-model)()) 
(defmodel true-model ()())
(defmodel indirect-model (true-model)())


(defun cv-test-engine ()
  ;;
  ;; before we get to engines, a quick check that we are correctly enforcing the
  ;; requirment that classes defined by defmodel inherit from model-object
  ;;
  (cv-assert (make-instance 'non-model))
  (cv-assert (make-instance 'true-model))
  (cv-assert (make-instance 'indirect-model))
  (cv-assert (handler-case
                 (progn
                   (make-instance 'faux-model)
                   nil) ;; bad to reach here
               (t (error) (trc "error is" error)
                 error)))
  ;; --------------------------------------------------------------------------
  ;; -- make sure non-cell slots still work --
  ;;
  ;; in mop-based implementations we specialize the slot-value-using-class accessors
  ;; to make cells work. rather than slow down all slots where a class might have only
  ;; a few cell-mediated slots, we allow a class to pick and choose which slots are cell-mediated.
  ;; 
  ;; here we make sure all is well in re such mixing of cell and non-cell, by exercising first
  ;; the reader and then the writer.
  ;;
  ;; the read is not much of a test since it should work even if through some error the slot
  ;; gets treated as if it were cell. but the setf will fail since cell internals reject changes
  ;; to cellular slots unless they are c-variable. (why this is so has to do with efficiency,
  ;; and will be covered when we get to cells being optimized away.)
  ;; 
  (cv-assert
   (eql :gas (fuel (make-instance 'engine :fuel :gas))))
  (cv-assert
   (eql :diesel (setf (fuel (make-instance 'engine :fuel :gas)) :diesel)))
  ;;
  ;;
  #+noterror ;; Cloucell needed to hold a Cell in a non cellular slot. duh.
  (cv-assert
   (handler-case
       (progn
         (make-instance 'engine :fuel (cv :gas))
         nil) ;; bad to reach here
     (t (error) (trc "error is" error)
       error)))
  ;;
  ;; ---------------------------------------------------------------------------
  ;; (1) reading cellular slots (2) instantiated as constant, variable or ruled
  ;;
  ;; aside from the simple mechanics of successfuly accessing cellular slots, this
  ;; code exercises the implementation task of binding a cell to a slot such that
  ;; a standard read op finds the wrapped value, including a functional value (the c?)
  ;;
  ;; aside; the cell pattern includes a transparency requirement so cells will be
  ;; programmer-friendly and in turn yield greater productivity gains. below we /initialize/
  ;; the cylinders cell to (cv 4) and then (c? (+ 2 2)), but when you read those slots the
  ;; cell implementation structures are not returned, the value 4 is returned.
  ;; 
  ;; aside: the value 4 itself occupies the actual slot. this helped when we used Cells
  ;; with a persistent CLOS tool which maintained inverse indices off slots if asked.
  ;;
  (cv-assert
   (progn
     (eql 33 (cylinders (make-instance 'engine-w-initform)))))
  
  (cv-assert
   (eql 4 (cylinders (make-instance 'engine :cylinders 4))))
  
  (cv-assert
   (eql 4 (cylinders (make-instance 'engine :cylinders (cv 4)))))
  
  (cv-assert
   (eql 4 (cylinders (make-instance 'engine :cylinders (c? (+ 2 2))))))
  
  (cv-assert
   (eql 16 (valves (make-instance 'engine
                     :cylinders 8
                     :valves (c? (* (cylinders self) (valves-per-cylinder self)))
                     :valves-per-cylinder (c? (floor (cylinders self) 4)))))) ;; admittedly weird semantics
  
  ;; ----------------------------------------------------------
  ;;  initialization echo
  ;;
  ;; cells are viewed in part as supportive of modelling. the echo functions provide
  ;; a callback allowing state changes to be manifested outside the dataflow, perhaps
  ;; by updating the screen or by operating some real-world device through its api.
  ;; that way a valve model instance could drive a real-world valve.
  ;;
  ;; it seems best then that the state of model and modelled should as much as possible
  ;; be kept consistent with each other, and this is why we "echo" cells as soon as they
  ;; come to life as well as when they change.
  ;;
  ;; one oddball exception is that cellular slots for which no echo is defined do not get echoed
  ;; initially. why not? this gets a little complicated.
  ;;
  ;; first of all, requires evaluation of a ruled cell. by checking first if a cell even is echoed
  ;; we can defer the evaluation of any ruled cell bound to an unechoed slot until such a slot is 
  ;; read by other code. i call this oddball because it is a rare slot that is
  ;; neither echoed nor used directly or indirectly by an echoed slot. but i have had fairly
  ;; expensive rules on debugging slots which i did not want kicked off until i had to check their
  ;; values in the inspector.
  ;;
  
  (macrolet ((echo-init (newv cylini)
               `(progn
                  (echo-clear 'cylinders)
                  (echo-clear 'valves)
                  (to-be (make-instance 'engine :cylinders ,cylini :valves ,cylini))
                  (cv-assert (echoed 'cylinders))
                  (cv-assert (eql ,newv (echo-new 'cylinders)))
                  ;(cv-assert (not (echo-old-boundp 'cylinders)))
                  ;(cv-assert (not (echoed 'valves)))
                  )))
    (echo-init 6 6)
    (echo-init 10 (cv 10))
    (echo-init 5 (c? (+ 2 3)))
    )
  
  ;; ----------------------------------------------------------------
  ;;   write cell slot
  ;;
  ;; for now only variable cells (slots mediated by c-variable structures) can be
  ;; modified via setf. an exception (drifter cells) may get resurrected soon. but as mentioned
  ;; above, an optimization discussed below requires rejection of changes to cellular slots
  ;; instantiated without any cell, and for purity the cell engine rejects setf's of slots mediated
  ;; by ruled cells. the idea being that we want the semantics of a ruled
  ;; cell to be fully defined by its rule, not arbitrary setf's from anywhere in the code.
  ;;
  ;; aside: variable cells can be setf'ed from anywhere, a seeming loss of semantic
  ;; control by the above purist view. but variables exist mainly to allow inputs to a dataflow model
  ;; from outside the model, usually in an event-loop processing os events, so spaghetti dataflow
  ;; should not follow from this.
  ;;
  ;; that said, in weak moments i resort to having the echo of one cell setf some other variable cell, 
  ;; but i always think of these as regrettable gotos and maybe someday i will try to polish them out 
  ;; of existence test.
  ;;
  ;;-------------------------
  ;;
  ;; first verify acceptable setf...
  ;;
  (cv-assert
   (let ((e (make-instance 'engine :cylinders (cv 4))))
     (setf (cylinders e) 6)
     (eql 6 (cylinders e))))
  ;;
  ;; ...and two not acceptable...
  ;;
  (cv-assert
   (handler-case
       (let ((e (make-instance 'engine :cylinders 4)))
         (setf (cylinders e) 6)
         nil) ;; bad to reach here
     (t (error)
       (trc "error correctly is" error)
       (cell-reset)
       t))) ;; something non-nil to satisfy assert
  
  (cv-assert
   (handler-case
       (let ((e (make-instance 'engine :cylinders (c? (+ 2 2)))))
         (setf (cylinders e) 6)
         nil) ;; bad to reach here
     (t (error) (trc "error correctly is" error) t)))
  
  (cv-test-propagation-on-slot-write)
  (cv-test-no-prop-unchanged)
  
  ;;
  ;; here we exercise a feature which allows the client programmer to override the default
  ;; test of eql when comparing old and new values. above we defined nonsense slot mod3 (unechoed)
  ;; and mod3ek (echoed) with a custom "unchanged" test:
  ;;

  ;;
  (let ((e (to-be
            (make-instance 'engine
              :mod3 (cv 3)
              :mod3ek (cv 3)
              :cylinders (c? (* 4 (mod3 self)))))))
    
    (cv-assert (eql 12 (cylinders e)))
    (echo-clear 'mod3)
    (echo-clear 'mod3ek)
    (trc "mod3 echoes cleared, setting mod3s now")
    (setf (mod3 e) 6
      (mod3ek e) 6)
    ;;
    ;; both 3 and 6 are multiples of 3, so the engine guided by the above
    ;; override treats the cell as unchanged; no echo, no recalculation
    ;; of the cylinders cell
    ;;
    (cv-assert (not (echoed 'mod3ek))) ;; no real need to check mod3 unechoed
    (cv-assert (eql 12 (cylinders e)))
    ;;
    ;; now test in the other direction to make sure change according to the 
    ;; override still works.
    ;;
    (setf (mod3 e) 5
      (mod3ek e) 5)
    (cv-assert (echoed 'mod3ek))
    (cv-assert (eql 20 (cylinders e)))
    )
  )

(defun cv-test-propagation-on-slot-write ()
  ;; ---------------------------------------------------------------
  ;;   propagation (echo and trigger dependents) on slot write
  ;;
  ;; propagation involves both echoing my change and notifying cells dependent on me
  ;; that i have changed and that they need to recalculate themselves.
  ;;
  ;; the standard echo callback is passed the slot-name, instance, new value,
  ;; old value and a flag 'old-value-boundp indicating, well, whether the new value
  ;; was the first ever for this instance.
  ;;
  ;; the first set of tests make sure actual change is handled correctly
  ;;
  (echo-clear 'cylinders)
  (echo-clear 'valves)
  (echo-clear 'valves-per-cylinder)
  (when *stop* (break "stopped!"))
  (let ((e (to-be (make-instance 'engine
                    :cylinders 4
                    :valves-per-cylinder (cv 2)
                    :valves (c? (* (valves-per-cylinder self) (cylinders self)))))))
    ;;
    ;; these first tests check that cells get echoed appropriately at make-instance time (the change
    ;; is from not existing to existing)
    ;;
    (cv-assert (and (eql 4 (echo-new 'cylinders))
                    (not (echo-old-boundp 'cylinders))))
    
    (cv-assert (valves-per-cylinder e)) ;; but no echo is defined for this slot
    
    (cv-assert (valves e))
    ;;
    ;; now we test true change from one value to another
    ;;
    (setf (valves-per-cylinder e) 4)
    ;;    
    (cv-assert (eql 16 (valves e)))
    ))

(defun cv-test-no-prop-unchanged ()
  ;;
  ;; next we check the engines ability to handle dataflow efficiently by /not/ reacting
  ;; to coded setfs which in fact produce no change.
  ;;
  ;; the first takes a variable cylinders cell initiated to 4 and again setf's it to 4. we
  ;; confirm that the cell does not echo and that a cell dependent on it does not get
  ;; triggered to recalculate. ie, the dependency's value has not changed so the dependent
  ;; cell's cached value remains valid.
  ;;
  (cell-reset)
  (echo-clear 'cylinders)
  (let* ((*dbg* t)
         valves-fired
         (e (To-be (make-instance 'engine
                     :cylinders (cv 4)
                    :valves-per-cylinder 2
                    :valves (c? (setf valves-fired t)
                              (trc "!!!!!! valves")
                              (* (valves-per-cylinder self) (cylinders self)))))))
    (trc "!!!!!!!!hunbh?")
    (cv-assert (echoed 'cylinders))
    (echo-clear 'cylinders)
    (cv-assert (not valves-fired)) ;; no echo is defined so evaluation is deferred
    (trc "sampling valves....")
    (let ()
      (cv-assert (valves e)) ;; wake up unechoed cell
      )
    (cv-assert valves-fired)
    (setf valves-fired nil)
  
    (cv-assert (and 1 (not (echoed 'cylinders))))
    (setf (cylinders e) 4) ;; same value
    (trc "same cyl")
    (cv-assert (and 2 (not (echoed 'cylinders))))
    (cv-assert (not valves-fired))
  
    (setf (cylinders e) 6)
    (cv-assert (echoed 'cylinders))
    (cv-assert valves-fired)))

#+test

(cv-test-engine)