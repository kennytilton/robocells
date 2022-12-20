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

#|

(defparameter *p* (player-n 0 0))

(inspect (player-n 0 0))
(inspect (player-n 0 1))
(inspect (player-n 0 3))
(inspect (player-n 1 0))
(inspect (player-n 1 1))

;-----------------------------------------

(dolist (team (kids *g*))
  (dolist (p (kids team))
    (player-send *p* "(turn 90)")
    ))

(excl:run-shell-command *server-exe* :wait nil)

(socket::lookup-port 6000 "udp")


;;;(deftask test-turns ()
;;;  ()
;;;  :achieved (let ((b (^ball)))
;;;              (ptrc nil "shadow achieved??"
;;;                :ball (seen-dir b)
;;;                :pos (ptr (^pos)) :neck .neck. :sightline (dgr (^sight-line)))
;;;              nil)
;;;  :subtasks nil
;;;  :attempt (unless .if-attempted.
;;;             '((turn_neck -45)(turn 75))))

|#

(deftask pirouhette () ;; a testing task
  ((state :initform :init :accessor state))
  :achieved (progn
              #+notyet (unless (and (< 8 (car (^pos)) 12)
                                 (< -2 (cdr (^pos)) 2))
                         (break "bad pos ~a" self))
              (ptrc nil "pirouhette" (ptr (^pos)) :bodydir (dgr .body-dir.)
                ;; :using (cddr (^field-o))
                :speed (cons .p-speed. .p-speed-dir.)
                :goal (easy-sight))

              nil)
  :attempt (with-std-attempt ()
             (if (zerop (mod .cycle 5)) ;; spiral actually
                '(turn 45)
              '(dash -100))
  #+snsnnsns (progn
               (case (state task)
                 (:init
                  (when
                      (> .p-speed. 0.2)
                    (setf (state task) :dashing))
                  (msg$ "(dash -100)"))
                                          
                 (:dashing
                  (setf (state task) :coasting)
                  (msg$ "(turn 90)"))))))


(let (last-sense last-see)
  (defun tmsg (p type msginfo &aux (now (now)))
    (declare (ignorable p msginfo))
    (ecase type
      (:sense
       (when (and last-sense last-see)
         (format t "~&~5d sense now ~d nextsee: ~d" now (- now last-see)
           (cond 
            ((>= (- now last-see) 100) :immediate )
            ((>= (- now last-see) 50) :half-cycle)
            (t :one-cycle+half))))
       (setf last-sense now))
      (:see
       (when last-see
         (format t "~&~5d see after sense ~d" now (- now last-sense)))
       (setf last-see now)))))