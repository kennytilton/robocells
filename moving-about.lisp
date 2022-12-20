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


(deftask find-ball ()
  ()
  :achieved (progn
              (bIf (b (^ball))
                (progn
                  (ptrc nil "find-ball BINGO> bodydir, neck" (dgr .body-dir.) .neck. b)
                  ;;(< (abs (seen-dir b) ) 20)
                  b)
                (progn
                  (ptrc nil "find-ball NOPE> " (dgr .body-dir.) .neck.)
                  )
              ))
  :attempt (with-std-attempt (:try-when (or (^sight-line self (fArcChange (radians 10)))
                                          (^attempt-ct task (fZerop))))
             (ptrc nil "find-ball attempt #" (1+ (attempt-ct task))
               :pos  (ptr (^pos))
               :bodydir .body-dir.
               :sightline(dgr (^sight-line)) 
               :trigger .cause
               )
             (list 'turn (floor .visible-angle. 2)))
  )

(defun fArcChange (sensitivity)
  (mksynapse (lastrelaybasis)
    :fire-p (lambda (syn newbasis)
              (declare (ignorable syn))
              (eko (nil "delta fire-p")
		   (or (not (and newbasis lastrelaybasis))
		       (let ((delta (abs (radians (degrees (- newbasis lastrelaybasis))))))
			 (> delta sensitivity)))))
    
    :relay-value (lambda (syn newbasis)
                   (declare (ignorable syn))
                   (prog1
                       (eko (nil "fArcChg decides"
                             lastrelaybasis newbasis
                             sensitivity (when lastrelaybasis
                                           (abs (radians (degrees (- newbasis lastrelaybasis))))))
                         (or (not (and newbasis lastrelaybasis))
			     (let ((delta (abs (radians (degrees (- newbasis lastrelaybasis))))))
			       (> delta sensitivity))))
                     (setf lastrelaybasis newbasis)))))

(deftask study-lines ()
  ()
  :achieved (> (attempt-ct task) 20)
  :attempt (with-std-attempt (:try-when (^real-time (sys-clock *g*) (fSensitivity 200)))
             (ptrc "study-lines: line, pos, dir"
               (car (seen-category self 'lines))
               :pos (^pos) :bodydir .body-dir.

;;;               (case (second (car (car (seen-category self 'lines))))
;;;                       (l :left)((t) :top)(r :right)(b :bottom))
;;;
;;;               :linedir (seen-dir )
;;;               
;;;               :sightlines (dgr (^sight-line)) (line-sight-line (car (seen-category self 'lines)))

               )
             (cond 
              ((zerop (attempt-ct task)) '(turn -30))
              ((< (abs (py (^pos))) 37) '(dash 50))
              (t '(turn 10))))
  )



(cells::def-c-trace find-ball attempt :c?)

(deftask turn-once ()
  ((delta :initarg :delta :reader delta))
  :achieved nil
  :attempt (with-std-attempt ()
             (ptrc "turn-once attempt" .body-dir. (delta task))
             (list 'turn (delta task))))



(deftask sense-body ()
  ()
  :achieved (with-singular-achievement
                (^sensed))
  :attempt (with-std-attempt (:try-when (^server-reached))
             'sense_body))

(deftask move-to-kickoff-position ()
  ()
  :subtasks'(sense-body)
  :achieved (with-singular-achievement
                (eko (nil "MTKPOS achieved?"
                      ) (pts-within (^pos) (kickoff-pos self) .5))) ;; /// endless loop if kickoff pos illegal
  :attempt (with-std-attempt ()
               (let ((pos (if (typep-pt (kickoff-pos self))
                              (kickoff-pos self)
                            (if (our-side-p) (first (kickoff-pos self))
                              (second (kickoff-pos self))))))
                 (without-c-dependency
                  (when (plusp (attempt-ct task)) (break "mtk twice? pos ~a" (^pos)))
                  `((turn_neck ,(- .neck.))(move ,(car pos) ,(cdr pos)))))))

(deftask turn-neck ()
  ((to-angle :initarg :to-angle :initform 0 :accessor to-angle))
  :achieved (when (^sensed)
              (let ((a (approx 1 .neck. (to-angle task))))
                (ptrc "turn-neck achieved?" (approx 1 .neck. (to-angle task)) .neck. a)
                a))
  :attempt (with-std-attempt ()
             (when (^sensed)
               (let ((delta (- (to-angle task) .neck.)))
                 (ptrc "turning neck" :amt delta :neck .neck. :to-angle (to-angle task))
                 `(turn_neck ,delta)))))
  
(deftask head-for-ball () ;; literally set body heading for ball
  ()
  :achieved (bIf (ball (^ball))
              (progn
                (ptrc nil "head-for-ball satisfied?" .neck. ball)
                (< (+ .neck. (seen-dir ball)) 10))
              (break "bad find-ball?"))
  :subtasks (list (mktask find-ball))
  :attempt (with-std-attempt ()
             (bif (ball (^ball))
               (let ((correction (+ .neck. (seen-dir ball))))
                 (ptrc nil "head-for-ball correcting" correction)
                 `(turn ,correction))
               (break "bad find-ball?"))))

(deftask approach-ball ()
  ((within :cell t :initarg :within :reader within
     :initform (c? (kickrange 40))))
  :wait-on-subtasks-p nil
  :achieved (bwhen (ball (^ball))
              (ptrc nil "ab satisfied?" #+nnn (pfldo) (< (seen-dist ball) (within task))
                :within (within task) :ball (seen-dist ball) (seen-dir ball) :pspeed-dir .p-speed. .p-speed-dir.)
              (when (and (< (seen-dist ball) (within task))
                      (< (abs (seen-dir ball)) 30)) ;; this close, no problem if at an angle
                (ptrc "approach-ball satisfied!" (pfldo) :ball ball :player .p-speed. .p-speed-dir.)
                t)) ;; /// worry about angle of attack and side of ball
  :subtasks (list (mktask head-for-ball)
              ;;(mktask avert-obstacles)
              )
  :attempt (with-std-attempt ()
             (bwhen (ball (^ball)) ;; we don't wait on subtasks
             (ptrc nil "approach att"
               ;; :neck .neck.
               :pdv (cons .p-speed-dir. .p-speed.)
               :bds (cons (seen-dir ball) (seen-dist ball))
               :b-dd (seen-ddist ball))
             (if (or (< (abs (+ .neck. (seen-dir ball))) 5) ;; /// allow for dist to ball and lead the ball; also, use .speed-dir.?
                   (< (seen-dist ball) 2)) ;; no last second turns
                 (bwhen (dash (cond
                               ((> (seen-dist ball) 3) 100)
                               ((> (seen-dist ball) 2) 50)
                               ((> (seen-dist ball) 1) 20)
                               ((and (seen-ddist ball)
                                  (> (seen-ddist ball) -.05)) 10)
                               ))
                   (ptrc nil "ab dash:" dash (seen-dist ball)(seen-ddist ball))
                   `(dash ,dash))
               (let ((turn (+ .neck. (seen-dir ball))))
                 (ptrc nil "ab turn" turn :ball ball)
                 `(turn ,turn))))))

(deftask move-outside-ball () ;; this could cover corner-kick, provided offset
  ()
  :subtasks (list (mktask find-ball))
  :achieved (> (abs (cdr (^pos))) 34) ;;/// 34 s/b global constant
  :attempt (with-std-attempt ()
             (let ((bp (seen-pos self (^ball))))
               `(move ,(floor (round (car bp)))
                  ,(floor (round (+ (cdr bp) (signum (cdr bp)))))))))
