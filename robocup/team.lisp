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

(defmodel team (family)
  ((name$ :initform nil :initarg :name$ :reader name$)
   (coach :cell t :initarg :coach :accessor coach :initform nil)
   (line-up :cell t :initarg :line-up :accessor line-up :initform nil))
  (:default-initargs
      :kids (c? (let ((last-pno 1))
                  (loop for position in (^line-up)
                      collect (destructuring-bind (role kickoff-pos task-factory) position
                                (make-instance 'player
                                  :role role
                                  :u-num (case (car role)
                                           (goalie 1)
                                           (otherwise
                                            (incf last-pno)))
                                  :kickoff-pos kickoff-pos
                                  :kids (c? (theKids (funcall task-factory self))))))))))


#+test
(team-establish "Yankees")
#|
(defun team-ensure (team$ &aux (et (existing-teams *g*)))
  (unless (find team$ et :test 'string-equal)
    (if (< (length et) 2)
        (team-establish team$)
      (break "Server already knows about two teams: ~a; no room for ~a" et team$))))

(defun team-establish (team$)
  (trc "Establishing team" team$)
  (dotimes (n 11)
    (dotimes (step 2)
      (let ((socket (make-instance 'rc-socket :send-port 6000)))
        (if (zerop step)
            (rc-socket-exchange socket (rcx$ (conc$ "init "
                                    team$
                                    (when (zerop n)
                                      " (goalie)")
                                    " (version 9)")))
          (progn
            (rc-socket-exchange socket (rcx$ (format nil "reconnect ~a ~d" team$ (1+ n))))
            (rc-socket-exchange socket (rc$ "bye"))))
        (rc-socket-close socket)))))

(defun p-send-off (team$ u-num)
  (let ((socket (make-instance 'rc-socket :send-port 6000)))
    (rc-socket-exchange socket (rcx$ (format nil "reconnect ~a ~d" team$ u-num)))
    (rc-socket-exchange socket (rc$ "bye"))
    (rc-socket-close socket)))

|#

(defun team-line-up ()
  '(((goalie) (-50 . -20))
;;;    ((fwd center) ((-10 . -5)(-10 . 0))) ;; kicking,receiving
;;;    ((fwd left) ((-2 . -10) (-5 . -10)))
;;;    ((fwd right) ((-2 . 10) (-5 . 10)))
;;;    ((mid left tackle)((-10 . -5)(-10 . -20)))
;;;    ((mid left guard)((-10 . -5)(-10 . -10)))
;;;    ((mid right tackle)((-10 . -5)(-10 . 20)))
;;;    ((mid right guard)((-10 . -5)(-10 . 10)))
;;;
;;;    ((back left)((-10 . -5)(-20 . -10)))
;;;    ((back center)((-10 . -5)(-20 . 0)))
;;;    ((back right)((-10 . -5)(-20 . 10)))
    ))