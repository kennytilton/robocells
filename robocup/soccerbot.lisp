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


(defmodel soccer-client (client family)
  (
   ;; --- internals support for synch with server ---
   ;;
   (cycle :initform -1 :accessor cycle)
   (last-exclusive-send :initform nil :accessor last-exclusive-send
     :documentation "internal real time during cycle zero, then the cycle")
   (current-time :initform 0 :accessor current-time
     :documentation "internal real time, updated in socket polling")
   ;;
   ;; --- perception ---
   ;;
   (sensed$ :initform (make-server-buffer) :accessor sensed$)
   (seen$ :initform (make-server-buffer) :accessor seen$)

   (sensed :cell t :initform (cv nil) :accessor sensed)
   (seen :cell t :initform (cv nil) :accessor seen)
   (heard :cell :ephemeral :initform (cv nil) :accessor heard)
   (server-error :cell :ephemeral :initform (cv nil) :accessor server-error)
   ;;
   ;; --- cognition ---
   ;;
   (play-mode :cell t :accessor play-mode
     :initform (c? (or (bwhen (h (^heard))
                         (destructuring-bind (who word) (cdr h)
                           (if (eql who 'referee)
                               word
                             .cache.)))
                     .cache.)))
   ;;
   ;; --- action ---
   ;;
   (act-now-p :cell :ephemeral :initform t :initarg :act-now-p :accessor act-now-p)
   )
  (:default-initargs
      :kids (c? (thekids (soccer-client-tasks self)))))


(defun bot-process-all-pending-msgs (self)
  (macrolet ((msg-is (id$)
               `(string-equal ,id$ msg$ :start2 1 :end2 (min (length msg$)
                                                           (1+ ,(length id$))))))
    (setf (real-time (sys-clock *g*)) (now))
    (server-buffer-clear (seen$ self))
    (server-buffer-clear (sensed$ self))
    (ptrc nil "bot-process-all-pending-msgs BEGIN" self)
    
    (do ((timeout 0 0) ;;(/ 200 1000) (/ 200 1000))
         (got 0 (1+ got))) ;;///try smaller first-time waits
        (#-allegro nil ;; assumes non-blocking read
	       #+allegro 
          (progn
            (ptrc nil "checking messages" self timeout)
            (let ((avail (mp::wait-for-input-available (udp-socket (socket self))
							    :timeout timeout)))
              (ptrc nil "message avail" avail self)
              (not avail)))
         #+okok (when (zerop got)(trc "no got")#+slo(sleep 1))
         #+chya (when (zerop got)
                  (let ((nogot (- (now) *last-receive-time*)))
                    (when (> nogot 500)
                      (trc "nogot" nogot)
                      (dotimes (n 10) (trc "nogot" nogot self (socket self)))
                      )))
         #+nahh (unless (< (- (now) *last-receive-time*) 6000)
                  (break "~dms since ~a last heard from the server"
                    (- (now) (last-receive-time (socket self))) self)
                  (throw :game-over self)))
      
      (let ((msg$ (rc-socket-read (socket self))))
        (when (zerop (length msg$))
          #+allegro (trc "whoa: avail but zero length" self)
          (return))
        (setf (real-time (sys-clock *g*)) (now))

        (trc nil "time now" (now))
        (trc nil "RECV>" self (subseq msg$ 0 (min (length msg$) 10)))

        (cond
         ((msg-is "sense_body")
          (trc nil "cache new sense_body" self (length msg$))
          (ncopy-server-buffer (sensed$ self) msg$)
          )

         ((msg-is "see") 
          (ptrc nil "cache new seen" self (length msg$))
          (ncopy-server-buffer (seen$ self) msg$))
           
         (t (trc nil "raw read other" self (length msg$))
           (msg$-digest self msg$))))))

  ;; /// for now, act-now-p is triggered by sense_body, so do any see first
  ;;
  (when (plusp (length (seen$ self)))
    (trc nil "looking at seen$" (left$ (seen$ self) 20)))
  (msg$-digest self (seen$ self))

  (when (plusp (length (sensed$ self)))
    (trc nil "looking at sensed$" (sensed$ self)))
  (msg$-digest self (sensed$ self))

  )
 

(defun msg$-digest (self msg$)
  (when (plusp (length msg$))
    (trc nil "msg$-digest>" :size (length msg$)
	 (left$ msg$ (min 10 (length msg$)))) ;;self (subseq msg$ 0 (min (length msg$) 40)))
    (msg-digest self (read-from-string msg$ t))))

(defmethod msg-digest (self msg)
  (case (car msg)
    (init
     (ecase (second msg)
       (ok (setf (server-reached self) t))))
    
    (server_param (unless *bot-doing-globals*
                    (trc "doing globals" self)
                    (setf *bot-doing-globals* self)
                    (setf *server* (server-pairs-storable (cdr msg)))))
    
    (player_param (when (eq self *bot-doing-globals*)
                    (setf *player-params* (server-pairs-storable (cdr msg)))))
    
    (player_type (when (eq self *bot-doing-globals*)
                   (push (server-pairs-storable (cdr msg)) (player-types *g*))))
    
    (hear
     (ptrc "hearing" self (cddr msg))
     (setf (heard self) (cons (now) (cddr msg))))

    (error (case (second msg)
             ((no_more_team_or_player_or_goalie reconnect)
              (setf (server-error self) (second msg)))
             (otherwise
              (break "Server error for ~a ~a" self (cdr msg)))))
    
    (otherwise (break "soccer-client ~a cannot handle msg: ~a" self msg))))



