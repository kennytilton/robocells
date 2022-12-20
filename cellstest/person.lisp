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

(defvar *name-ct-calc* 0)

(defmodel person ()
  ((speech :cell :ephemeral :initform (cv "hello, world") :initarg :speech :accessor speech)
   (thought :cell :ephemeral :initform (c? (speech self)) :initarg :thought :accessor thought)
   (names :cell t :initform nil :initarg :names :accessor names)
   (pulse :cell t :initform nil :initarg :pulse :accessor pulse)
   (name-ct :cell t
            :initarg :name-ct :accessor name-ct
            :initform (c? "name-ct" 
                          (incf *name-ct-calc*)
                          (length (names self))))))

(def-c-echo names ((self person) new-names)
  (format t "~&you can call me ~a" new-names))

(defmethod c-unchanged-p ((self person) (slotname (eql 'names)) new-value old-value)
  (equal new-value old-value))

(defvar *thought* "less")

(def-c-echo thought ((self person) new-value)
  (when new-value
    (setq *thought* new-value)
    (trc "i am thinking" new-value)))

(def-c-echo speech ())

(defmodel sick ()
  ((e-value :cell :ephemeral :initarg :e-value :accessor e-value)
   (s-value :cell t :initarg :s-value :reader s-value)))

(def-c-echo s-value () 
  :test)

(def-c-echo e-value () 
  :test)

(defun cv-test-person ()
  (cv-test-person-1)
  (cv-test-person-2)
  (cv-test-person-3)
  (cv-test-person-4)
  (cv-test-person-5)
  (cv-test-talker)
  )

(defun cv-test-person-1 ()
  ;; 
  ;; a recent exchange with someone who has developed with others a visual
  ;; programming system was interesting. i mentioned my dataflow thing, he mentioned
  ;; they liked the event flow model. i responded that events posed a problem for
  ;; cells. consider something like:
  ;;
  ;; (make-instance 'button
  ;;      :clicked (cv nil)
  ;;      :action (c? (when (clicked self) (if (- (time-now *cg-system*) (last-click-time.....
  ;;
  ;; well, once the button is clicked, that cell has the value t. the rest of the rule executes
  ;; and does whatever, the rule completes. finis? no. the time-now cell of
  ;; the system instance continues to tick-tick-tick. at each tick the action cell gets triggered,
  ;; and (here is the problem) the clicked cell still says t.
  ;;
  ;; the problem is that clicked is event-ish. the semantics are not "has it ever been clicked",
  ;; they are more like "when the /passing/ click occurs...". we could try requiring the programmer
  ;; always to execute:
  ;;
  ;;     (setf (clicked it) t)
  ;;     (setf (clicked it nil)
  ;;
  ;; ...but in fact cells like this often are ruled cells which watch mouse actions and check if the
  ;; mouse up was in the control where the mousedown occurred. so where to put a line of code
  ;; to change clicked back to nil? a deep fix seemed appropriate: teach cells about events, so...
  ;;
  ;; cellular slots can be defined to be :ephemeral if the slot will be used for
  ;; event-like data. [defining slots and not cells as ephemeral means one cannot arrange for such a 
  ;; slot to have a non-ephemeral value for one instance and ephemeral values for other instances. we 
  ;; easily could go the other way on this, but this seems right.] 
  ;;
  ;; the way ephemerals work is this: when a new value arrives in an ephemeral slot it is echoed and 
  ;; propagated to dependent cells normally, but then internally the slot value is cleared to nil.
  ;; thus during the echo and any dataflow direct or indirect the value is visible to other code, but
  ;; no longer than that. note that setting the slot back to nil bypasses propagation: no echo, no 
  ;; triggering of slot dependents.
  ;;
  ;;
  (let ((p (md-make 'person :speech (cv nil))))
    ;;
    ;; - ephemeral c-variable cells revert to nil if setf'ed non-nil later
    ;;
    (setf (speech p) "thanks for all the fish")
    (cv-assert (null (speech p)))
    (cv-assert (equal (echo-new 'speech) "thanks for all the fish"))
    (cv-assert (equal *thought* "thanks for all the fish")) ;; thought is ephemeral as well, so tricky test
    ;;
    ;; now check the /ruled/ ephemeral got reset to nil
    ;;
    (cv-assert (null (thought p)))))

(defun cv-test-person-2 ()
  ;;
  ;; a sick case: an ephemeral initialized to non-nil
  ;;

  (let ((p (md-make 'sick
             :e-value (cv 2)
             :s-value (c? (when (e-value self)
                            (* 10 (e-value self)))))))
    ;;
    ;; what happens here is that during instance-initialization cell slots get echoed initially
    ;; according to some order not guaranteed by cell semantics. in the amoeba implementation
    ;; the order is determined by the order in which slots appear here:
    ;;
    ;;;    (def-c-shared-init (sick)
    ;;;        (e-value s-value))
    ;;
    ;; so e-value gets processed first, and thus before the s-value rule has been invoked and
    ;; established the dependency of s-value on e-value. so when e-value takes on the value 2
    ;; the engine sees no dependencies on e-value and does not kick off s-value. it does echo
    ;; the value, and then sets it to nil.
    ;;
    ;; when s-value gets its initial echo next, it sees a nil e-value and returns nil itself,
    ;; and the dependency is established. so s-value begins nil, but then correctly becomes
    ;; 20 when e-value is set once again to be 2.
    ;;
    (cv-assert (null (s-value p)))
    (setf (e-value p) 2)
    (cv-assert (eql 20 (s-value p))))
  )

(defun cv-test-person-3 ()
  ;; -------------------------------------------------------
  ;;  dynamic dependency graph maintenance
  ;;
  ;; dependencies of a cell are those other cells actually accessed during the latest
  ;; invocation of the rule. note that a cellular slot may be constant, not mediated by a
  ;; cell, in which case the access does not record a dependency.
  ;;
  (let ((p (md-make 'person
             :names (cv '("speedy" "chill"))
             :pulse (cv 60)
             :speech "nice and easy does it"
             :thought (c? (if (> (pulse self) 180)
                              (concatenate 'string (car (names self)) ", slow down!")
                            (speech self))))))
    ;;
    ;; with the (variable=1) pulse not > 80, the branch taken leads to (constant=0) speech, so:
    ;;
    (cv-assert (eql 1 (length (cd-useds (md-slot-cell p 'thought)))))
    ;;
    ;; with the (variable=1) pulse > 80, the branch taken leads to (variable=1) names, so:
    ;;
    (setf (pulse p) 200)
    (cv-assert (eql 2 (length (cd-useds (md-slot-cell p 'thought)))))
    ;;
    ;; let's check the engine's ability reliably to frop dependencies by lowering the pulse again
    ;;
    (setf (pulse p) 50)
    (cv-assert (eql 1 (length (cd-useds (md-slot-cell p 'thought)))))))

(defun cv-test-person-4 ()
  (let ((p (md-make 'person
             :names '("speedy" "chill")
             :pulse (cv 60)
             :speech (c? (car (names self)))
             :thought (c? (when (< (pulse self) 100) (speech self))))))
    ;;
    ;; now let's see if cells are correctly optimized away when:
    ;;
    ;;    - they are defined and
    ;;    - all cells accessed are constant.
    ;;
    (cv-assert (null (md-slot-cell p 'speech)))
    (cv-assert (md-slot-cell-flushed  p 'speech))
    (cv-assert (c-optimized-away-p (md-slot-cell-flushed p 'speech)))   
    
    (cv-assert (not (c-optimized-away-p (md-slot-cell p 'thought)))) ;; pulse is variable, so cannot opti
    (cv-assert (eql 1 (length (cd-useds (md-slot-cell p 'thought))))) ;; but speech is opti, so only 1 used
    ))

(defun cv-test-person-5 ()
  ;;
  ;; for now cells do not allow cyclic dependency, where a computation of a cell leads back
  ;; to itself. we could do something like have the self-reference return the cached value
  ;; or (for the first evaluation) a required seed value. we already have logic which says
  ;; that, if setf on a variable cell cycles back to setf on the same cell we simply stop, so
  ;; there is no harm on the propagation side. but so far no need for such a thing.
  ;;
  ;; one interesting experiment would be to change things so propagation looping back on itself
  ;; would be allowed. we would likewise change things so propagation was breadth first. then
  ;; state change, once set in motion, would continue indefinitely. (propagation would also have to
  ;; be non-recursive.) we would want to check for os events after each propagation and where
  ;; real-time synchronization was necessary do some extra work. this in contrast to having a timer 
  ;; or os null events artificially move forward the state of, say, a simulation of a physical system. 
  ;; allowing propagation to loop back on itslef means the system would simply run, and might make
  ;; parallelization feasible since we already have logic to serialize where semantically necessary.
  ;; anyway, a prospect for future investigation.
  ;;
  ;;   make sure cyclic dependencies are trapped:
  ;;
  (cv-assert
   (handler-case
       (progn
         (pulse (md-make 'person
                  :names (c? (maptimes (n (pulse self))))
                  :pulse (c? (length (names self)))))
         nil)
     (t (error)
        (trc "error" error)
        t)))
  )
;;
;; we'll toss off a quick class to test tolerance of cyclic

(defmodel talker8 ()
  (
   (words8 :cell t :initform (cv8 "hello, world") :initarg :words8 :accessor words8)
   (idea8 :cell t :initform (cv8 "new friend!") :initarg :idea8 :accessor idea8)))

(defmodel talker ()
  ((words :cell t :initform (cv "hello, world") :initarg :words :accessor words)
   (idea :cell t :initform (cv "new friend!") :initarg :idea :accessor idea)))

(def-c-echo words ((self talker) new-words)
  (trc "new words" new-words)
  (setf (idea self) new-words))

(defmethod c-unchanged-p ((self talker) (slotname (eql 'words)) new-value old-value)
  (string-equal new-value old-value))

(def-c-echo idea ((self talker) new-idea)
  (trc "new idea" new-idea)
  (setf (words self) new-idea))

(defmethod c-unchanged-p ((self talker) (slotname (eql 'idea)) new-value old-value)
  (string-equal new-value old-value))

(def-c-echo words8 ((self talker) new-words8)
  (trc "new words8" new-words8)
  (setf (idea8 self) new-words8))

(defmethod c-unchanged-p ((self talker) (slotname (eql 'words8)) new-value old-value)
  (string-equal new-value old-value))

(def-c-echo idea8 ((self talker) new-idea8)
  (trc "new idea8" new-idea8)
  (setf (words8 self) new-idea8))

(defmethod c-unchanged-p ((self talker) (slotname (eql 'idea8)) new-value old-value)
  (string-equal new-value old-value))

(defmacro cv-assert-error (&body body)
  `(cv-assert
    (handler-case
        (prog1 nil
          ,@body)
     (t (error)
        (trc "error" error)
        t))))

(defun cv-test-talker ()
  ;;
  ;; make sure cyclic setf is trapped
  ;;
  (cell-reset)
  (cv-assert-error
   (let ((tk (make-instance 'talker)))
     (setf (idea tk) "yes")
     (string-equal "yes" (words tk))
     (setf (words tk) "no")
     (string-equal "no" (idea tk))))
  ;;
  ;; make sure cells declared to be cyclic are allowed
  ;; and halt (because after the first cyclic setf the cell in question
  ;; is being given the same value it already has, and propagation stops.
  ;;
  (let ((tk (make-instance 'talker8)))
     (setf (idea8 tk) "yes")
     (string-equal "yes" (words8 tk))
     (setf (words8 tk) "no")
     (string-equal "no" (idea8 tk)))
  )