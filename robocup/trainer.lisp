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

(defmodel supervisor (soccer-client)
  ((ok :cell t :initform (cv nil) :initarg :ok :accessor ok))
  (:default-initargs
      :init-port 6001
    :server-init '(init (version 9))))

(defmethod trcp ((self supervisor))
  t)

(defmodel coach (supervisor)
  ((team :cell t :initform (cv nil) :initarg :team :accessor team)))

(defmethod print-object ((self coach) s)
  (format s "~a coach" (team self)))

(defmodel trainer (supervisor)
  ((game :cell t :initform (cv nil) :initarg :game :accessor game)))


(deftask drill-2 ()
  ()
  :achieved (every #'achieved (^kids task))
  :subtasks (list
             (mktask greet-referee)
             (mktask run-trials :trials 5)))



(defun gon () ;; aka, main()
  (when *g*
    (not-to-be *g*))
    
  (setf *g* nil
    *server* nil
    *player-params* nil
    *bot-doing-globals* nil)
  
  (cell-reset)
  
  (to-be
   (setq *g*
         (make-instance 'practice-session
           :set-up `((ball . (10 30))
                     (trainer drill-2)
                     (end-of-drill . ,(lambda (practice)
                                      (bwhen (goalie (car (kids (car (kids practice)))))
                                        (bwhen (task (car (kids goalie)))
                                          (and (typep task 'goalie-shadow-ball)
                                            (achieved task))))))
                     (teams
                      ("TriLisp" ;; "RoboCells"
                       :line-up (((goalie) (-50 . -30) goalie-training)
                                 ;((fwd center) (-10 . 0) fwd-ctr-training)
                                 ;((fwd left) (-5 . -10) fwd-training)
                                 ;((fwd right) (-5 . 10) fwd-training)
                                 ))
                      #+not
                      ("TeamKenny"
                       :line-up (((goalie) (-10 . 30) goalie-training)
                                 ;((fwd center) (-10 . 0) fwd-ctr-training)
                                 ;((fwd left) (-5 . -10) fwd-training)
                                 ;((fwd right) (-5 . 10) fwd-training)
                                 )))))))
  
  (trc "start cycling")
        
  (server-message-loop))

(defmethod msg-digest :around ((self supervisor) msg)
  (ptrc nil "msg-digest" self (car msg) (cadr msg))
  (case (first msg)

    (ok (ecase (second msg)
          (look (destructuring-bind (cycle &rest objects) (cddr msg)
                  (setf (cycle self) cycle)
                  (trc nil "bingo seen ~a ~a" (now) (find 'p (cdr (^seen)) :key 'caar)) ;;objects)
                  (setf (seen self) (cons (now) objects))))
          (team_names ;; (break "teamnames ~a" msg)
            (setf (existing-teams *g*)
              (mapcar (lambda (team)
                        (trc nil "record existing team" (third team))
                        (symbol-name (third team))) ;; n.b: case info has been lost
                (cddr msg))))
          ((say move recover change_mode start)))
      (trc nil "setting ok" self (second msg))
      (setf (ok self) (second msg)))

    (otherwise (call-next-method))))

(defun goalie-training (self)
  (case (^play-mode)     
    ((nil) (mktask report-in))
    ((before_kick_off)
     (list 
      (mktask move-to-kickoff-position)
      (mktask find-ball)))
    (kick_off_l #+not (mktask study-lines) (mktask goalie-shadow-ball))))

(defun fwd-ctr-training (self)
  (case (^play-mode)     
    ((nil) (mktask report-in))
    ((before_kick_off)
     (list 
      (mktask move-to-kickoff-position)
      (mktask find-ball)))
    ((kick_off_l play_on) (mktask kick-ball-towards-goal))
    ))

(defun fwd-training (self)
  (case (^play-mode)     
    ((nil) (mktask report-in))
    ((before_kick_off)
     (list 
      (mktask move-to-kickoff-position)
      (mktask find-ball)))
    (play_on (mktask kick-ball-towards-goal))))




