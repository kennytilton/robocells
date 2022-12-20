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

(eval-when (compile load)
  (defun radians (d) (/ d (/ 180 pi))))

(defun degrees (r) 
  (* r (/ 180 pi)))

(defun radian-degrees (rads) (* rads (/ 180 pi)))

(defun sqr (x) (* x x))

;;; coordinates
;;;
(defun coord-dist (x1 y1 x2 y2)
  (sqrt (+ (expt (- x2 x1) 2)(expt (- y2 y1) 2))))

(defun coord-dir-between (x1 y1 x2 y2)
  (if (zerop (- x1 x2))
      (if (> y1 y2)
;;;          #.(radians -90)
          (radians -90)
;;;        #.(radians 90))
        (radians 90))
    (+ (atan (/ (- y2 y1)(- x2 x1)))
      (if (< x2 x1)
          (if (< y2 y1)
              (- pi)
            pi)
        0))))




;;; --- points ---
;;;
(defun make-pt (x y) (cons x y))

(defun px (pt) (car pt))
(defun (setf px) (newv pt)
  (setf (car pt) newv))

(defun py (pt) (cdr pt))
(defun (setf py) (newv pt)
  (setf (cdr pt) newv))

(defun copy-pt (pt)
  (make-pt (px pt)(py pt)))

(defun typep-pt (pos)
  (and (consp pos)
    (numberp (px pos))
    (numberp (py pos))))

(defun pt- (p1 p2)
  (make-pt (- (px p1)(px p2))
    (- (py p1)(py p2))))

(defun pts-within (p1 p2 delta)
  (and p1 p2 (>= delta (pt-dist p1 p2))))

(defun pt-dist (p1 p2)
  (sqrt (+ (expt (- (px p1)(px p2)) 2)
             (expt (- (py p1)(py p2)) 2))))

(defun pt-dead-reckon (pt dir dist &aux (rads (radians dir)))
  (make-pt
   (+ (px pt)(* dist (cos rads)))
   (+ (py pt)(* dist (sin rads)))))

(defun pt-reflect (side pt)
  "Robocup-specific xlate between absolute and relative"
  (npt-reflect side (copy-pt pt)))

(defun npt-reflect (side pos)
  (ecase side
    (l (setf (py pos) (- (py pos))))
    (r (setf (px pos) (- (px pos)))))
  pos)

;;; --- vectors ---

(defun make-vc (dir dist)
  (cons dir dist))
(defun vc-dir (vc) (car vc))
(defun vc-dist (vc) (cdr vc))

(defun vc-between (from to)
  (make-vc
   (pt-dir-between from to)
   (pt-dist from to)))

(defun pt-dir-between (from to)
  (coord-dir-between (px from)(py from)(px to)(py to)))

#+test
(list
 (dgr (pt-dir-between (make-pt 0 0) (make-pt 4 4)))
 (dgr (pt-dir-between (make-pt 0 0) (make-pt -4 4)))
 (dgr (pt-dir-between (make-pt 0 0) (make-pt -4 -4)))
 (dgr (pt-dir-between (make-pt 0 0) (make-pt 4 -4)))
 )

;; --- rectangles ---
;;

(defun make-rct (l tp r b)
  (cons (make-pt l tp) (make-pt r b)))

(defun rtl (rct)(car rct))
(defun rbr (rct)(cdr rct))

(defun rl (rct)(px (rtl rct)))
(defun rt (rct)(py (rtl rct)))
(defun rr (rct)(px (rbr rct)))
(defun rb (rct)(py (rbr rct)))

(defun pt-slope (p1 p2)
  (/ (- (py p2)(py p1))
    (- (px p2)(px p1))))

;; --- y = mx + b ---------
;;

(defun make-line-mxb (m b)
  (cons m b))

(defun mxb-m (mxb)(car mxb))
(defun mxb-b (mxb)(cdr mxb))

(defun make-mxb-two-point (p1 p2)
  (let ((m (pt-slope p1 p2)))
    (make-line-mxb m (- (py p1) (* m (px p1))))))

(defun mxb-solve-when-x (mxb x)
  (+ (* (mxb-m mxb) x) (mxb-b mxb)))

(defun mxb-solve-when-y (mxb y)
  (/ (- y (mxb-b mxb))
    (mxb-m mxb)))



;; --- fancy calcs ---


(defun pt-in-rct (pt rct)
  (and (<= (rl rct)(px pt)(rr rct))
    (<= (rb rct)(py pt)(rt rct))))




(defun quadratic (a b c)
  (let ((rad (sqrt (- (sqr b) (* 4 a c))))
        (mb (- b))
        (aa (+ a a)))
    (trc nil "quad> out rad, mb aa:" rad mb aa)
    (list (/ (+ mb rad) aa)
      (/ (- mb rad) aa))))
