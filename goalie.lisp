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

(defun goalie-plan (self)
  (ptrc "goalie-plan> entry" self)
  (or (case (^play-mode)
        (before_kick_off
         (ptrc "goalie-plan> for befkickkoff" self)
         (list 
          (mktask move-to-kickoff-position)
          (mktask study-lines) ;; find-ball)
          ))

        (kick_off_l (mktask goalie-shadow-ball))

        ((goalie_catch_ball_l goalie_catch_ball_r)
         (when (our-side-p)
           (mktask kick-ball-towards-goal)))

        ((goal_kick_l goal_kick_r) ;; /// test this
         (when (our-side-p)
           (mktask kick-ball-towards-goal))))
    (mktask goalie-shadow-ball)))


(deftask goalie-shadow-ball ()
  ((fld-lim :initform (LANDPOINT-LOCATION '(F L T)) :reader fld-lim)
   (shadowing-pos :cell t :reader shadowing-pos
     :initform (c? (let ((self (player self)) ;; /// do macros with-task and with-task-deferred
                         )
                     (bWhen (b (^ball))
                       (let ((bpos (pt-dead-reckon (^pos) (+ (dgr (^sight-line)) (seen-dir b)) (seen-dist b))))
                         (ptrc nil "pt-dead-reckon ball:" (ptr bpos) :player (ptr (^pos))
                           :sight-line (dgr (^sight-line))
                           :body-dir (dgr .body-dir.) :neck .neck.
                           :seen-dir-dist (seen-dir b)(seen-dist b))
                         
                         (if (pt-in-penalty-area bpos)
                             bpos
                           (let* ((shot-mxb (make-mxb-two-point bpos (make-pt *goal-line-ours* 0)))
                                  (pa-top-cross-y (mxb-solve-when-x shot-mxb *pa-top-ours*))
                                  )
                             
                             (if (<= *pa-left* pa-top-cross-y *pa-right*)
                                 (let ((shpos (make-pt *pa-top-ours* pa-top-cross-y)))
                                   (ptrc nil "top cross pa top, deadreckon, shot-mxb"
                                     (ptr shpos) (ptr bpos) shot-mxb)
                                   shpos)
                               (let* ((pa-side-cross-y (* *pa-left* (signum (py bpos))))
                                      (pa-side-cross-x (mxb-solve-when-y shot-mxb pa-side-cross-y)))
                                 (let ((shpos (make-pt pa-side-cross-x pa-side-cross-y)))
                                   (ptrc "side cross pa" (ptr shpos) :bady pa-top-cross-y (ptr bpos))
                                   shpos))))))))))
   )
  :achieved (progn
              (trc nil "shadow achieved??" (seen-dir (^ball)) (ptr (^pos)) .neck. (dgr (^sight-line)))
              (bwhen (p (^pos))
                (bwhen (s (^shadowing-pos task))
                  (ptrc nil "checking shadow ach: pos,shadow" (ptr p)(ptr s)(< (pt-dist p s) 1))
                  (< (pt-dist p s) 1))) ;; filter within a few yards, depending on distance
              )
  :subtasks (list 
             
             (mktask find-ball)
             ;;(mktask avert-obstacles)
             )
  
  :attempt (let ((last-heading-check))
             (with-std-attempt (:try-when (^seen self (fDifferent)))
               (ptrc nil "shadow att entry!!!!!: cycle, checkheading" (car (^seen)) last-heading-check)
               (bwhen (shpos (^shadowing-pos task))
                 (let* ((shv (vc-between (^pos) shpos))
                        (turn-to-spot (if (and last-heading-check
                                            (< (- (car (^seen)) last-heading-check) 5))
                                          0
                                        (let ((tot-turn (turn-between-headings .body-dir. (vc-dir shv))))
                                          (setq last-heading-check (car (^seen)))
                                          (ptrc nil "tot-turn=" (dgr tot-turn)
                                            :=minus :tospot (dgr (vc-dir shv))
                                            :bodydir (dgr .body-dir.))
                                          tot-turn))))
                   (if (> (abs turn-to-spot) (radians 10))
                       (let* ((b (^ball))
                              (dgr-to-spot (degrees turn-to-spot))
                              (neck-turn (round (- (if b (seen-dir b)
                                                     (progn (trc "whoa!!!!!!! no ball???")
                                                       0))
                                                  dgr-to-spot))))
                         (ptrc nil "turn dgr-to-spot:" dgr-to-spot :neck-adj neck-turn
                           :seendir (when b (seen-dir b)))
                         `((turn ,(round dgr-to-spot))
                           (turn_neck ,neck-turn)))
                     (bif (dash (cond
                                 ((> (vc-dist shv) 3) 100)
                                 ((> (vc-dist shv) 2) 100)
                                 ((> (vc-dist shv) 1) 50)))
                       (progn
                         (ptrc nil "shadow ab dash:" dash (vc-dist shv))
                         (format nil "dash ~d" dash))
                       (trc "no dash!!!!! for vc-dist" (vc-dist shv)))))))))

(defun fCycled (&optional (times 1))
  (mksynapse (prior-cycle)
    :fire-p (lambda (syn seen &aux (new-cycle (car seen)))
              (declare (ignorable syn))
              (trc "fCycled<fire>: prior,new" (>= new-cycle (+ prior-cycle times))
                prior-cycle new-cycle)
              (>= new-cycle (+ prior-cycle times)))
    
    :relay-value (lambda (syn seen &aux (new-cycle (car seen)))
                   (declare (ignorable syn))
                   (trc "fCycled<relay>: prior,new" (not (eql new-cycle prior-cycle))
                     prior-cycle times new-cycle)
                   (when (or (null prior-cycle)
                           (>= new-cycle (+ prior-cycle times)))
                     (setf prior-cycle new-cycle)
                     seen))
    ))


(defun turn-between-headings (from to)
  (let ((naive (mod (- to from) (* 2 pi))))
    (cond
     ((> naive pi) (- naive pi pi))
     ((< naive (- pi)) (+ naive pi pi))
     (t naive))))

(deftask catch-ball ()
  ()
  :subtasks (list (mktask approach-ball)) ;; add constraint to say "within penalty area"
  :achieved (^got-ball)
  :attempt (with-std-attempt ()
             (bIf (ball (^ball))
               (if (<= (seen-ddist ball) (server-param catchable_area_l))
                   (if (> (seen-dir ball) 5)
                       (progn
                         (trc "turning to face ball before catch"
                           (seen-dir ball) (server-param catchable_area_w))
                         (rc$ "turn %d" (seen-dir ball)))
                     (rc$ "catch %d" (seen-dir ball)))
                 (break "bad approach?"))
               (break "bad find?"))))

