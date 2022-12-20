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

(defun approx (delta v1 v2)
  (<= (abs (- v1 v2)) delta))

(defmodel timer ()
  ((real-time :cell t :initform (cv 0) :accessor real-time)))

(defun minimize-abs (v max-av)
  (* (signum v) (min (abs v)(abs max-av))))

(defmacro average (&rest args)
  `(/ (+ ,@args) ,(length args)))

(define-symbol-macro .cause
    cells::*cause*)

(defun ptr (pt)
  (when pt
    (cons (round (car pt))(round (cdr pt)))))

(defun dgr (r) (when r (round (degrees (cond
                                        ((> r pi) (- r (+ pi pi)))
                                        ((< r (- pi)) (+ r (+ pi pi)))
                                        (t r))))))

(defun now ()
   (* 1000 (/ (get-internal-real-time) internal-time-units-per-second)))

(defun gather (value list &key (key #'identity)(test #'eql))
  (loop for elem in list
       when (when (funcall test value (funcall key elem))
              elem)
      collect it))

(define-symbol-macro .cycle
    (cycle self))

(defmacro ptrc (&rest trcargs)
  (unless (eql (car trcargs) 'nil)
    (let* ((fmt$ (find-if #'stringp trcargs))
           (ff$ (conc$ "[~a] " fmt$))
           (timing (gensym)))
      `(without-c-dependency
        (let ((,timing (if (plusp .cycle) .cycle (format nil "~,3f" (/ (mod (now) 100000) 1000)))))
          ,(if (stringp (car trcargs))
               `(when (trcp self)
                  (cells::call-trc (format nil ,ff$ ,timing) ,@(cdr trcargs)))
             `(when (trcp ,(first trcargs))
                (cells::call-trc (format nil ,ff$ ,timing) ,@(cddr trcargs)))))))))



(defun irt$ ()
  (format nil "irt: ~6,3f" (/ (mod (now) 100000) 1000)))


(defun abs> (v1 v2 delta)
  (assert (plusp delta))
  (> (sep v1 v2) delta))

(defun sep (v1 v2) (abs (- v1 v2)))