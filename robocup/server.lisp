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

(defmacro server-param (param)
  `(rc-item *server* ',param))

(defmacro msg$ (fmt$ &rest fmt-args)
  `(format nil ,fmt$ ,@fmt-args))

(define-symbol-macro .ball-size.
    (server-param ball_size))

(define-symbol-macro .visible-angle.
    (server-param visible_angle))

(define-symbol-macro .goal-width.
    (server-param goal_width))

#| version 9 |#

#+test
'(init l 1 before_kick_off)

(progn
  (defun make-rc-item-storage (size)
    (declare (ignore size))
    nil)
  (defmacro rc-item-store (store keyform value )
    (let ((key (gensym))(entry (gensym)))
      `(let ((,key ,keyform))
         (bif (,entry (assoc ,key ,store))
           (rplacd ,entry ,value)
           (push (cons ,key ,value) ,store)))))
  (defun rc-item (store key )
    (cdr (assoc key store))))

#+faster
(progn
  (defun make-rc-item-storage (size)
    (make-hash-table :size size))
  (defun (setf rc-item) (value store key)
    (setf (gethash key store) value))
  (defun rc-item (store key)
    (gethash key store)))

(defun server-pairs-storable (server-data-pairs
                             &optional (store (make-rc-item-storage (+ 5 (length server-data-pairs)))))
  (dolist (p server-data-pairs store)
    ;;(trc "server info:" p)
    (case (first p)
      (sense_body
       (rc-item-store store 'view-quality (second p))
       (rc-item-store store 'view-width (third p)))
      (stamina
       (rc-item-store store 'stamina (second p))
       (rc-item-store store 'effort (third p)))
      (speed
       (rc-item-store store 'speed-amt (second p))
       (rc-item-store store 'speed-dir (third p)))
      (otherwise
       (rc-item-store store (car p) (cadr p))))))


