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

#-allegro
(eval-when (compile load)
   (require :db-sockets))

#-allegro
(eval-when (compile load)
   (require :db-sockets))



(defpackage :robocup
    (:nicknames :rcup)
    (:use
     #:common-lisp
     #-(or cormanlisp cmu) #:clos
     #:cells
     #-allegro #:sockets
     )
  (:export))

(in-package :robocup)

#+cmu
(defun rbd (&optional force)
  (progn ;; with-compilation-unit (:optimize '(optimize (speed 2)(compilation-speed 2)(safety 1)))
    (asdf:oos 'asdf:load-op :robocells :force force)))

(defparameter *g* nil)
(defparameter *last-receive-time* 0)
(defparameter *last-send-time* nil)

(defparameter *server-exe* "D:\\RoboCup\\rcsoccersim-win-9.3.5\\rcssserver\\rcssserver.exe -sfile server.conf -pfile player.conf")
(defparameter *server* nil) ;; the params, actually
(defparameter *player-params* nil)
(defparameter *bot-doing-globals* nil)
(defparameter *thinking* nil)

(defmodel game (family)
  ((over-p :cell t :initform (cv nil) :initarg :over-p :accessor over-p)
   (set-up :cell t :initform nil :initarg :set-up :accessor set-up)
   (player-types :cell t :initform (cv nil) :accessor player-types)
   (sys-clock :cell t :initform (c? (make-instance 'timer)) :reader sys-clock)
   (players-on-field :initform nil :accessor players-on-field)
   (existing-teams :cell t :initform (cv :undetermined) :accessor existing-teams)
   (trainer :cell t :initarg :trainer :accessor trainer :initform nil)))

(defun set-up-get (set-up key)
  (cdr (assoc key set-up)))

(defmodel practice-session (game)
  ((trainer :cell t :initarg :trainer :accessor trainer :initform nil)
   (end-of-drill :cell t :initform t :initarg :end-of-drill :accessor end-of-drill))
  (:default-initargs
      :trainer (c? (bwhen (trainer-tasks (set-up-get (^set-up) 'trainer))
                     (to-be (make-instance 'trainer
                              :game self
                              :kids (c? (mapcar (lambda (task)
                                                  (make-instance task
                                                    :player self)) trainer-tasks))))))
    :over-p (c? (every 'achieved (kids (trainer self))))
    :end-of-drill (c? (funcall (set-up-get (^set-up) 'end-of-drill) self))
    :kids (c? (mapcar (lambda (team)
                        (destructuring-bind (name$ &key coach line-up) team
                          ;; (team-ensure name$)
                          (declare (ignore coach))
                          (make-instance 'team
                            :name$ name$
                            :line-up line-up)))
                (set-up-get (^set-up) 'teams)))))

(def-c-echo sys-clock ()
  (when old-value
    (not-to-be old-value))
  (when new-value
    (to-be new-value)))

(defun server-message-loop ()
  (let ((*last-receive-time* (now))
        (*last-send-time* (now))
        (*print-pretty* nil)
        (loop-start (now))
        )
    (declare (ignorable loop-start))
    (do ((*thinking* nil nil)
         (loops 0 (1+ loops))
         )
        ((or *stop*
           (over-p *g*)
           #+nahh (when (> (- (now) *last-receive-time*) 3000) ;; to end a test, just kill server
                    (trc "no mo receive: now, last:" (now) *last-receive-time*)
                    t)))
      #+shh (when (and (plusp loops) (zerop (mod loops 10)))
              (trc "one loop per" (round (floor (- (now) loop-start) loops))))
      (trc nil "--- new coach/players cycle ---" :team-sizes (mapcar (lambda (tm)
                                                                               (length (kids tm)))
                                                                       (kids *g*)))
      
      (bwhen (trainer (trainer *g*))
          (bot-process-all-pending-msgs trainer))
      (dolist (team (kids *g*))
        (bwhen (coach (coach team))
          (bot-process-all-pending-msgs coach))
        (dolist (p (kids team))
          (bot-process-all-pending-msgs p)))

      (trc nil "fell out of msg loop" *thinking*)
      
      (when *thinking*
        (dolist (team (kids *g*))
          (dolist (p (kids team))
            (trc "announcing done" p)
            (rc-socket-send (socket p) (serverize 'done))))))))




