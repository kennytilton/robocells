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


(deftask greet-referee ()
  ()
  :achieved (and (^server-reached)
              (eql 7 (length (player-types *g*))))
  :attempt (with-std-attempt ()
             '(init (version 9))))

(deftask set-up-drill ()
  ()
  :achieved (with-singular-achievement
                (and (^attempt-ct task (fPlusp))
                  (every (lambda (team)
                           (every (lambda (p)
                                    (and (eql 'before_kick_off (play-mode p)) ;; they got the message
                                      (every 'achieved (kids p))))
                             (kids team)))
                    (kids *g*))))
  :attempt (with-std-attempt ()
             `((change_mode before_kick_off)
               (move (ball) ,@(set-up-get (set-up (game self)) 'ball))
               (recover))))

(deftask run-trials ()
  ((trials :cell t :initarg :trials :initform 1 :accessor trials)
   (completed :cell t :initarg :completed :accessor completed
      :initform (c...(0) (when (^end-of-drill (game (player self)))
                           1))))
  :subtasks (when (< (completed task)(trials task))
              (list (mkTask set-up-drill)))
  :achieved (= (trials task)(completed task))
  :attempt (with-std-attempt (:try-when (completed task))
             '(change_mode kick_off_l)))

(deftask inventory-field ()
  ()
  :achieved (^seen)
  :subtasks (list (mkTask get-teams))
  :attempt (with-std-attempt ()
             'look))

(deftask get-teams ()
  ()
  :achieved (not (eq :undetermined (existing-teams *g*)))
  :attempt (with-std-attempt ()
             'team_names))

;;;(deftask clear-field ()
;;;  ()
;;;  :subtasks (list
;;;             (mktask stop-play) ;; trouble once we have two coaches
;;;             (mktask inventory-field))
;;;  
;;;  :achieved .if-attempted.
;;;  #+risky (with-singular-achievement 
;;;                (not (ek "clearfield achieved if not:" find 'p (cdr (^seen)) :key 'caar)))
;;;  
;;;  
;;;  :attempt (with-std-attempt ()
;;;             (dolist (p-obj (cdr (^seen)) 'look)
;;;               (when (eql 'p (caar p-obj))
;;;                 (destructuring-bind (p name$ u-num &rest goalie-geo)
;;;                   (car p-obj)
;;;                   (declare (ignorable p goalie-geo))
;;;                   (ptrc "sending off" name$ u-num .cause)
;;;                   (p-send-off name$ u-num))))))
                   
(deftask stop-play ()
  ()
  :achieved (with-singular-achievement
                (trc "stop-play achieved> entry" .cache. .cause)
              (eq 'change_mode (^ok)))
  :attempt (with-std-attempt ()
             '(change_mode before_kick_off)))

