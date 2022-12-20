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

(in-package :robocup)
 
(defmodel task (family)
  ((player :initarg :player :reader player)
   (inactive :cell t :initarg :inactive :reader inactive
     :initform (c? (or (when (typep .parent. 'task)
                         (inactive .parent.))
                     (bwhen (s (psib))
                       (not (achieved s)))))
     :documentation "Avoid computations until prior tasks completed.")
   (trcp :initarg :trcp :initform nil :accessor trcp)
   (wait-on-subtasks-p :initarg :wait-on-subtasks-p :reader wait-on-subtasks-p :initform t)
   (waiting-on-subtasks :cell t :reader waiting-on-subtasks
     :initform (c? (when (wait-on-subtasks-p self)
                     (not (every #'achieved (^kids))))))
   (achieved-body :initarg :achieved-body :reader achieved-body :initform nil)
   (achieved :cell t :initarg :achieved :reader achieved
     :initform (c? (unless (or (^inactive)
                             (^waiting-on-subtasks))
                     (when (funcall (achieved-body self) cells::c)
                       #+shh (let ((task self)(self (player self)))
                         (ptrc "TASK> achieved" task self))
                       t))))

   (achieved-dbg :initform :unbound :accessor achieved-dbg)
   
   (attempt-ct :cell t :initarg :attempt-ct :accessor attempt-ct :initform (cv 0))
   (attempt :cell :ephemeral :initarg :attempt :accessor attempt :initform nil)
   ))

(defmacro task-lambda (code)
  `(lambda (cells::c &aux (task (cells::c-model cells::c))
             (self (player task)))
     (declare (ignorable cells::c self task))
     ,code))

(defun attempt-now (cells::c try-when-test &aux (self (cells::c-model cells::c)))
  (trc nil "attempt-now entry" .cause
    (not (null (act-now-p (player self))))
    (not (^inactive))
    (not (^waiting-on-subtasks))
    (not (^achieved)))
  (and
   (act-now-p (player self))
   (not (^inactive))
   (not (^waiting-on-subtasks))
   (not (^achieved))
   (bWhen (rt (eko (nil "try when decides")
                (funcall try-when-test cells::c)))
       #+chill (without-c-dependency
                (let ((task self)
                      (self (player self)))
                  (when (plusp (attempt-ct task))
                    (ptrc "TASK> retry!!" task self (cycle self) .cause))))
     t)))

(defmacro with-std-attempt ((&key (try-when '(^attempt-ct task (fZerop)))) &body attempt-body)
  `(c? (when (attempt-now cells::c (task-lambda ,try-when))
         (without-c-dependency
          (funcall (task-lambda (progn ,@attempt-body)) cells::c)))))

(defmethod print-object ((self task) s)
  (format s "~a(~a)" (md-name self) (achieved-dbg self)))

(def-c-echo achieved ((self task))
    (setf (achieved-dbg self) new-value))

(define-symbol-macro .if-attempted.
    (^attempt-ct task (fPlusp)))

(defmacro with-singular-achievement (&body code)
  `(or .cache. ,@code))

(defmacro deftask (name (&optional (supertask 'task))
                    mo-better-slots 
                    &key
                    trcp
                    (wait-on-subtasks-p t)
                    (achieved (break "Specify ':achieved NIL' explicitly to run indefinitely"))
                    attempt
                    subtasks
                    )
  `(defmodel ,name (,supertask)
     ,mo-better-slots
     (:default-initargs
         :trcp ,trcp
       :wait-on-subtasks-p ,wait-on-subtasks-p
       :kids (c? (let ((task self)
                       (self (player self)))
                   (declare (ignorable self task))
                   (mapcar (lambda (subtask)
                             (if (symbolp subtask)
                                 (make-instance subtask :player self)
                               subtask))
                     (theKids ,subtasks))))
       :achieved-body (lambda (cells::c &aux (task (cells::c-model cells::c))(self (player task)))
                        (declare (ignorable self task))
                        ,achieved)
       :attempt ,attempt)))

(defmacro mktask (taskname &rest iargs)
  `(make-instance ',taskname :player self ,@iargs))

(defmethod serverize ((msg string))
  (rcx$ msg))

(defmethod serverize ((msg symbol))
  (when msg ;; nil is a symbol
    (serverize (list msg))))

(defmethod serverize ((attempt cons))
  (if (atom (car attempt))
      (string-downcase (format nil "~a~c" attempt #\null))
    (mapcar #'serverize attempt)))

(def-c-echo attempt (task attempt)
  (when attempt
    (task-make-attempt task (serverize attempt))))

; -------------------------------------------
(defmethod task-make-attempt (task (attempt cons)
                               &optional defer-attempt-counting
                               &aux (self (player task)))
  (bif (duper (find-if (lambda (step)
                         (dup-for-cycle (player task) step))
                attempt))
      (progn
        (ptrc "dup-for-cycle!!!" duper))
    (progn
      (dolist (step attempt)
        (task-make-attempt task step t))
      (unless defer-attempt-counting
        (incf (attempt-ct task))))))

(defmethod task-make-attempt (task (attempt$ string)
                               &optional defer-attempt-counting
                               &aux (self (player task)))
  (if (dup-for-cycle self attempt$)
      (ptrc "Multiple not sent!!!" attempt$)
    (progn
      (when (plusp (cycle self))
        (assert (not *thinking*)))
      (ptrc nil "SEND>" self (remove #\null attempt$))
      (when (one-per-cycle-p attempt$)
        (setf (last-exclusive-send self) 
          (if (zerop (cycle self)) ;; /// start of second half?
              (now)
            (cycle self))))
        
      (rc-socket-send (socket self) attempt$)
      (unless defer-attempt-counting
        (trc nil "incremmenting attempt-ct: task, from" task (attempt-ct task) attempt$)
        (when (and (plusp (attempt-ct task))
                (typep task 'move-to-kickoff-position))
          (break "whassup?"))
        (incf (attempt-ct task))))))

(defun one-per-cycle-p (attempt$)
  (find-if (lambda (x-cmd$)
             (string= x-cmd$ attempt$ :start2 1 :end2 (length x-cmd$)))
    '("move" "turn" "dash" "kick" "catch")))

(defun dup-for-cycle (self attempt$)
  (when (one-per-cycle-p attempt$)
    (if (zerop (cycle self))
        (and (last-exclusive-send self)
          (< (- (now) (last-exclusive-send self)) 100))
      (eql (cycle self) (last-exclusive-send self)))))



