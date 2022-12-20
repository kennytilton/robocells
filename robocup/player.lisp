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

(defmodel player (soccer-client)
  (
   ;; player parameters
   ;;
   (role :reader role :initform '(fwd center) :initarg :role)
   (kickoff-pos :initform nil :reader kickoff-pos :initarg :kickoff-pos)
   
   (u-num :initform nil :initarg :u-num :accessor u-num)
   
   (side :cell t :accessor side ;; /// how/does will this change at half-time?
     :initform (cv nil))
   
   (player-type :cell t :accessor player-type
     :initform (c? (or .cache.
                     (ptrc nil "seeking p-type 0" (length (player-types *g*)))
                     (find 0 (player-types *g*)
                       :key (Lambda (type) (rc-item type 'id))))))
   
   (think :cell :ephemeral :initform (cv nil) :initarg :think :accessor think)
   
   ;; --- cognition ---
   ;;
   
   (field-orient-abs :cell t :accessor field-orient-abs
     :initform (c? (let ((foa2 (bwhen (seen (^seen))
                                 (multiple-value-list
                                  (where-am-i2? (cdr seen))))))
                     (if (null (car foa2))
                         (let ((foa (bwhen (seen (^seen))
                                      (multiple-value-list
                                       (where-am-i? (cdr seen))))))
                           (if (null (car foa))
                               (progn
                                 (when (^seen)
				   (trc "both where-am-is failed tho seen" (^seen)))
                                 .cache.)
			     foa))
                       foa2))))
   
   (pos :cell t :accessor pos
     :initform (c? (bwhen (abs-pos (first (^field-orient-abs)))
                     (let ((p (pt-reflect (^side) abs-pos)))
                       (ptrc nil "new pos" (ptr p))
                       (ptrc nil "pos c? pos: abs,rel" self (^side) (ptr abs-pos) (ptr p))
                       #+not (if (or (equal '(-3 . -37) (ptr p))
                                   (equal '(-10 . -5) (ptr p)))
                                 (trc "good rel pos ~a (abs= ~a) for ~a" (ptr p) (ptr abs-pos) self)
                               (break "bad rel pos ~a (abs= ~a) for ~a" (ptr p) (ptr abs-pos) self))
                       p))))
   
   (sight-line :cell t :accessor sight-line
     :initform (c? (bwhen (abs (second (^field-orient-abs)))
                     (let ((rel (- ;; rel coord system goes mirrors absolute!
                                 (if (eql 'r (^side))
                                     (+ abs pi)
                                   abs))))
                       (ptrc nil "wai? abs,rel dir:" (^side) abs rel )
                       (ptrc nil "new sight line, neck" (dgr rel) .neck.)
                       rel))))
   (ball :cell t :initarg :ball :reader ball
     :initform (c? (seen-category self 'ball)))
   
   (got-ball :cell t :initform (cv nil) :accessor got-ball) ;; caught, actually
   )
  (:default-initargs
      :init-port 6000
    :act-now-p (c? (if (or (null *server*)
                         (zerop (server-param synch_mode))) ;; <<< almost got split across two places
                       (or (^sensed) ;; /// enhance to wait for possible upcoming see
                         t)
                     (^think)))
    :server-init (c? (trc nil "server init rule entry" )
                   (format nil "(init ~a~a (version 9))~c"
                     (p-team$ self)
                     (if (eql 'goalie (car (role self)))
                         " (goalie)" "")
                     #\null))))

(defun seen-category (self category)
  (bwhen (s (^seen))
    (cdr (assoc category (cdr s)))))

(defun in-bounds-p (self &aux (pos (^pos)))
  (when pos
    (and (>= *field-left* (py pos) *field-right*)
      (<= *goal-line-ours* (px pos) *goal-line-theirs*))))

(defmethod soccer-client-tasks ((self player))
  (case (car (role self))
    (goalie (goalie-plan self))
    (fwd (forward-plan self))
    (otherwise (forward-plan self))))

(defun p-team$ (self)
  (if .parent. (name$ .parent.) "WalkOns"))

(define-symbol-macro .body-dir.
    (- (^sight-line) (radians .neck.)))

(defmacro our-side-p ()
  (let ((handled-l '(before_kick_off goal_kick_l goalie_catch_ball_l
                      corner_kick_l
                      kick_off_l free_kick_l kick_in_l))
        (handled-r '(goal_kick_r goalie_catch_ball_r
                      corner_kick_r kick_off_r free_kick_r
                      kick_in_r)))
    `(let ((mode (^play-mode)))
       (assert (or (find mode ',handled-l) (find mode ',handled-r)))
       (or (and (eq 'l (side self)) (find mode ',handled-l))
         (and (eq 'r (side self)) (find mode ',handled-r))))))

; --- basic tasks --------------------

(deftask report-in ()
  ()
  :achieved (^server-reached)
  :attempt (with-std-attempt ()
             #+chya (format nil "reconnect ~a ~d"
                      (p-team$ self)
                      (u-num self))
             (format nil "init ~a (version 9)"
               (p-team$ self)
               )))

;------ handy accessors --------------

(defun player-n (team player)
  (nth player (kids (nth team (kids *g*)))))

(define-symbol-macro .kickable-margin.
    (rc-item (player-type (nearest self player)) 'kickable_margin )) ;; /// lose "nearest"

(define-symbol-macro .player-size.
    (rc-item (player-type (nearest self player)) 'player_size ))

(defmacro kickrange (&optional (pct 100))
  `(+ (* .kickable-margin. (/ ,pct 100)) ;; closer = bigger wallop
     .player-size.
     .ball-size.))

(defmacro goal-sight ()
  `(and (^seen)
     (cdr (assoc 'goals (cdr (^seen))))))

(defmacro easy-sight ()
  `(and (^seen)
     (or (cdr (assoc 'goals (cdr (^seen))))
       (find-if (lambda (f)
                  (eql 'c (second (car f))))
         (cdr (assoc 'flags (cdr (^seen))))))))

(defun seen-pos (self sight)
  (cons (+ (car (^pos)) (cos (seen-dir sight)))
    (+ (cdr (^pos)) (sin (seen-dir sight)))))

(defmacro pfldo ()
  `(list (ptr (^pos)) (dgr .body-dir.)))

;--- boring stuff -------------------------------

(defmethod print-object ((self player) s)
  (format s "player ~a" (role self))
  #+not (format s "~a:~a" (or (when .parent. (kidno .parent.)) "?")
     (role self)))

(defmethod not-to-be :before ((self Player))
  (bwhen (s (socket self))
    (rc-socket-close s)))

;--- debug ---------------

(defmethod trcp ((self player))
  #+not (eql self (player-n 1 0))
  #+not (and (eql (side self) 'l)
    (equal (role self) '(goalie)))
  t)

(defun rc-stamp (cycle)
  cycle) ;; (if (zerop cycle) (now) cycle))

(defmethod msg-digest :around ((self player) msg)
  (ptrc nil "msg-digest" self (car msg))
  (case (car msg)
    (init (ecase (cadr msg)
            ((l r) (destructuring-bind (side u-num playmode) (cdr msg)
                     (ptrc "player sees init" (role self) side u-num playmode)
                     (setf
                      (server-reached self) t
                      (u-num self) u-num
                      (side self) side
                      (heard self) `(hear referee ,playmode))))))

    (reconnect (ecase (cadr msg)
                 ((l r) (destructuring-bind (side playmode) (cdr msg)
                          (ptrc "reconnected> port, msg" (send-port (socket self)) msg)
                          (setf (side self) side
                            (server-reached self) t
                            (heard self) `(hear referee ,playmode))))))
    
    (sense_body
     ;;(tmsg self :sense (cddr msg))
     (setf (cycle self) (second msg))
     (trc nil "sensing" self (cycle self))
     (setf (sensed self) (cons (rc-stamp (second msg))
                           (server-pairs-storable (cddr msg))))) ;; cddr skips cycle
    
    (see
     ;;(trc "seeing" (cddr msg))
     (setf (seen self) (cons (rc-stamp (second msg)) (see (cddr msg))))
     #+shh (bif (b (seen-category self 'ball))
             (ptrc "raw see of ball!!!!" b)
             (ptrc "NO BALL")))

    (think
     (trc "THINK>" self (now) (cycle self))
     (setf *thinking*
       (setf (think self) t)))
    
    (otherwise
     (call-next-method))))
