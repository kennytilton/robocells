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

(defun forward-plan (self)
  (case (^play-mode)
    ((before_kick_off)
     (list
      (mktask move-to-kickoff-position)
      (mktask find-ball)
      ))
            
    ((kick_off_l kick_off_r)
      (delete-if #'null
        (list
         (mktask move-to-kickoff-position)
         (mktask find-ball)
         (when (and (our-side-p)
                 (equal (role self) '(fwd center)))
           (mktask kick-off)))))

    ((free_kick_l free_kick_r)
     (when (and (our-side-p)
             (equal (role self) '(fwd center)))
         (mktask kick-off)))

    ((kick_in_l kick_in_r)
     (when (and (our-side-p)
             (equal (role self) '(fwd center)))
       (mktask kick-in)))

    ((corner_kick_l corner_kick_r)
     (when (and (our-side-p)
             (equal (role self) '(fwd center)))
       (mktask corner-kick)))

    (play_on (mktask kick-ball-towards-goal))
    (otherwise
     (mktask report-in))))


(deftask kick-off ()
  ((offset :initform 0 :initarg :offset :reader offset))
  :subtasks (list (mktask approach-ball))
  :achieved (eql (^play-mode) 'play_on)
  :attempt (with-std-attempt ()
             (trc "kickoff with player:" (pfldo)
                :ball (^ball) :pspeed (cons .p-speed. .p-speed-dir.))
              ;;(break)
              `(kick 50 45)
              ;;(msg$ "(kick 60 ~a)" (* 80 (if (zerop (random 2)) 1 -1)))
              ))

(deftask corner-kick ()
  ()
  :subtasks (list (mktask find-ball)
              (mktask move-outside-ball)
              (mktask approach-ball))
  :achieved .if-attempted.
  :attempt (with-std-attempt ()
             (let ((power 100)( dir (floor (* 95 (- (signum (cdr (^pos))))))))
               (trc nil "attack posts, power angle" power dir)
               `(kick ,power ,dir))))

(deftask kick-in () ;; this could cover corner-kick, provided offset
  ()
  :subtasks (list (mktask find-ball)
              (mktask move-outside-ball)
              (mktask kick-ball-towards-goal))
  :achieved t)

(deftask kick-ball-towards-goal ()
  ()
  :subtasks (list (mkTask approach-ball))
  :achieved (bwhen (ball (^ball))
                (> (seen-ddist ball) 3))
  :attempt (with-std-attempt ()
             (let* ((pl (attack-post self 'l))
                    (pr (attack-post self 'r)))
               (multiple-value-bind (power dir)
                   (bIf (shot-dir (cond
                                   ((and pr pl)
                                    (if (zerop (random 2))
                                        (+ (seen-dir pl) (/ (- (seen-dir pr)(seen-dir pl)) 10))
                                      (- (seen-dir pr) (/ (- (seen-dir pr)(seen-dir pl)) 10))))
                                   (pl (+ (seen-dir pl) 3))
                                   (pr (- (seen-dir pr) 3))))
                     (values (* 100 (/ (- 180 (abs shot-dir)) 180)) shot-dir)
                     (progn
                       (trc "cant see goal" self .body-dir.)
                       (values 20 (* 90 (signum .body-dir.))))) ;;(- .body-dir.)))
                 (trc "kicking!!" self :angl dir :power power)
                 `(kick ,(floor power) ,(floor (round dir)))))))

(defun attack-post (self post)
  (when (seen self)
    (assoc `(f g ,(ecase (side self) (l 'r)(r 'l))
              ,(ecase (side self)
                 (l (ecase post (l 't)(r 'b)))
                 (r (ecase post (l 'b)(r 't)))))
      (cdr (assoc 'flags (cdr (seen self))))
      :test #'equal)))