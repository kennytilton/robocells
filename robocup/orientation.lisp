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

(defparameter *field-left* 36)
(defparameter *field-right* -36)

(defparameter *pa-top-ours* -36)
(defparameter *goal-line-ours* -52.5)
(defparameter *goal-line-theirs* 52.5)
(defparameter *pa-top-theirs* 36)
(defparameter *pa-right* 20.16)
(defparameter *pa-left* -20.16)

(defun where-am-i? (seen)
  (when seen
    ;(trc "where-am-i? > got seen" seen seen)
    (destructuring-bind (sight-loc-1 sight-loc-2)
        (pick-two-landpoints seen)
      (trc nil "where-am-i? > picked two lpts" sight-loc-1 sight-loc-2)
      (when (and sight-loc-1 sight-loc-2)
        (landpoints-location sight-loc-1 sight-loc-2)))))

(defun where-am-i2? (seen)
  (bwhen (line (car (cdr (assoc 'lines (cdr seen)))))
    (let ((los (line-sight-line line)))
      (dolist (lp-nearest (cdr (assoc 'landpoints (cdr seen))))
        (bWhen (lp-nearest-loc (landpoint-location (car lp-nearest)))
          (return-from where-am-i2?
            (values
             (view-point (radians (- los (seen-dir lp-nearest))) (seen-dist lp-nearest) lp-nearest-loc)
             (radians los))))))))

(defun view-point (los dist pt)
  (let ((vx (- (px pt) (* dist (cos los))))
        (vy (- (py pt) (* dist (sin los)))))
    (trc nil "subtracting dx" (* dist (cos los)) :from-lpx (px pt)
      :and-dy (* dist (sin los)) :from-lpy (py pt))
    (make-pt vx vy)))

(defun landpoints-location (sight-loc-1 sight-loc-2)
  (destructuring-bind (id1 s1 d1 &rest junk)
      (car sight-loc-1)
    (declare (ignorable id1 junk))
    (destructuring-bind (id2 s2 d2 &rest junk)
        (car sight-loc-2)
      (declare (ignorable id2 junk))
      (viewpoint-location
       (radians d1) s1 (cdr sight-loc-1)
       (radians d2) s2 (cdr sight-loc-2)))))

(defun line-sight-line (line)
  (destructuring-bind ((l id) distance direction) line
    (declare (ignorable l distance))
    (ecase id
      (l (cond
          ((plusp direction) (+ direction 90))
          (t (- direction 90))))
      (r (cond
          ((minusp direction) (+ direction 90))
          (t (- direction 90))))
      ((t) (cond
            ((plusp direction) direction)
            (t (+ direction 180))))
      (b (cond
          ((minusp direction) direction)
          (t (+ direction 180)))))))


#+good
(VIEWPOINT-LOCATION 0.47123889803846897d0 11.1 '(0 . 0) 0.10471975511965977d0 46.1 '(36.0 . 0.0))

#+bad
(VIEWPOINT-LOCATION -0.17453292519943295d0 46.1 '(36.0 . 0.0)
  -0.6108652381980153d0 48.4 '(36.0 . 20.16))
#+fixx
(VIEWPOINT-LOCATION -0.17453292519943295d0 46.1 '(36.0 . 0.0)
  -0.6108652381980153d0 (pt-dist '(-10 . -5) '(36.0 . 20.16)) '(36.0 . 20.16))

;;;(dgr (pt-dir-between '(-10 . -5) '(36 . 0)))
;;;
;;;(dgr (pt-dir-between '(-10 . -5) '(36.0 . 20.16)))
;;;
;;;(sin (radians 30))
;;;
;;;(cos 30)
;;;
;;;(pt-dist '(-10 . -5) '(36 . 0))
;;;(pt-dist '(-10 . -5) '(36.0 . 20.16))
;;;
;;;(dgr -0.13962634015954636d0)
;;;(dgr -0.5759586531581288d0)


(defun viewpoint-location (dir1 dist1 xy1 dir2 dist2 xy2)
  (trc nil "vpl input dir1:" (dgr dir1) :dir2 (dgr dir2))
  (trc nil "vpl input :dist1" (round dist1) :xy1 xy1 :dist2 (round dist2) :xy2 xy2)

  (flet ((los-adj (v los) (- los v)))
    (let* ((v1 (* dist1 (sin dir1)))
           (v2 (* dist2 (sin dir2)))
           (v3 (- v2 v1))
           (h4 (- (* dist2 (cos dir2))(* dist1 (cos dir1))))
           (los ;; aka, line of sight (not body--need neck angle for that)
            (let ((los (+ (pt-dir-between xy1 xy2)
                         (if (< h4 0) pi 0)
                         (if (zerop h4)
                             (* (signum v1) (/ pi 2))
                           (atan (/ v3 h4))))))
              (trc nil "los sees (pt-dir-between xy1 xy2)" (dgr (pt-dir-between xy1 xy2)))
              (trc nil "los sees h4 (zerop h4)" h4 (zerop h4))
              (trc nil "los sees altitudes" :v1 (Round v1) :v2 (round v2) :vdelta (round v3))
              (trc nil "los ===>" (dgr los))
              los))
           (dir1z (los-adj dir1 los))
           (dx1 (* dist1 (cos dir1z)))
           (dy1 (* dist1 (sin dir1z))))
      (trc nil "dir1z" (dgr dir1z))
      (trc nil "dx1,dy1" dx1 dy1)

      (let ((a1 (make-pt
                 (- (px xy1) dx1)
                 (- (py xy1) dy1))))

        (trc nil "h4, los, delta, two answers are" :h4 h4 (round h4) :los (dgr los)
          (ptr (pt- a1 a2))(ptr a1) (ptr a2))

        ;;(print (list (dgr los) (ptr a1) (ptr a2)))
        (values a1 los)))))

(defun pick-two-landpoints (sights &aux best1 best2 bestsep usable)
  (dolist (sight (cdr (assoc 'landpoints sights)))
    (destructuring-bind (id dist &optional dir ddist ddir)
        sight
      (declare (ignore dist ddist ddir))
      (trc nil "sight1" sight)
      (unless dir ;; weird this: if no dir, really means no dist and dir is first value!!!!
        (return))

      (bwhen (lploc (landpoint-location id))
        (assert lploc) ;; not sure about this, but now I think all landpoints (f and g) are known
        (let ((pt-loc (car (push (cons sight lploc) usable))))
          (dolist (prior (cdr usable)) ;; cdr to skip the one we just pushed
            (destructuring-bind (id dist2 &optional dir2 ddist ddir)
                (car prior)
              (declare (ignore id dist2 ddist ddir))
              (let ((sep (sep dir dir2))) ;; /// work for closer as well as wider
                (when (> sep 20)
                  (trc nil "pick two> good sep" sep prior(car usable))
                  (return-from pick-two-landpoints (list prior pt-loc)))
                (when (or (null bestsep) (< sep bestsep))
                  (setf bestsep sep
                    best1 prior
                    best2 pt-loc)))))
          ))))
  ;; it happens (assert (and best1 best2)() "two not picked from ~a" (cdr (assoc 'landpoints sights)))
  (trc nil "pick-two> settling for weak sep!!!!!!:" bestsep (cdr (assoc 'landpoints sights)))
  (list best1 best2))

(defun landpoint-location (lpid &aux (xmax 52.5) (ymax 34))
  (let ((state 'init)
        x y)
    (dolist (lpid-atom lpid)
      (case state
        (init (case lpid-atom
                ((f g) (setq state lpid-atom))
                (otherwise (error "invalid landpoint type ~a" lpid-atom))))
        (g (case lpid-atom
             (l (return-from landpoint-location (cons (- xmax) 0)))
             (r (return-from landpoint-location (cons xmax 0)))
             (otherwise (error "invalid goal side ~a" lpid-atom))))
        (f (case lpid-atom
             (p (setq state 'l-or-r?
                    x (- xmax 16.5)
                    y 20.16))
             (g
              (assert *server* () "Cannot decide (f g l/r t/b)'s until server_params downloaded")
              (setq state 'goal-flag
                  x xmax
                  y (/ .goal-width. 2)))
             (c (setq state 'f-center))
             (l (setq x (- xmax) state 'fx-boundary))
             (r (setq x xmax state 'fx-boundary))
             ((t b) (setq y (* (if (eq 'b lpid-atom) -1 1) (+ 5 ymax))
                        state 'fy-boundary))
             ;;(b (setq y (- ymax) state 'fy-boundary))
             (otherwise (error "invalid flag id2 ~a" lpid-atom)))
          )
        (f-center (case lpid-atom
                    ((t) (return-from landpoint-location (cons 0 ymax)))
                    (b (return-from landpoint-location (cons 0 (- ymax))))
                    (otherwise (error "invalid center-flag t/b ~a" lpid-atom))))
        (l-or-r?
         (setq state 't-c-or-b?)
         (ecase lpid-atom
           (l (setq x (- x)))
           (r)))

        (t-c-or-b? 
         (return-from landpoint-location (cons x (* y (ecase lpid-atom ((t) 1)(c 0)(b -1))))))

        (goal-flag
         (setq state 't-c-or-b?)
         (ecase lpid-atom
           (l (setq x (- x)))
           (r)))
        
        (fx-boundary (ecase lpid-atom
                       (0 (return-from landpoint-location (cons (+ x (* 5 (signum x))) 0)))
                       ((t) (setq y ymax state 'fx-on-off?))
                       (b (setq y (- ymax) state 'fx-on-off?))))
        (fy-boundary (ecase lpid-atom
                       (0 (return-from landpoint-location (cons 0 y)))
                       ((l r) (setq x (ecase lpid-atom (l -1)(r 1))
                                  state 'fy-off))))
        (fy-off (ecase lpid-atom
                  ((10 20 30 40 50) (return-from landpoint-location (cons (* x lpid-atom) y)))))
        
        (fx-on-off? (ecase lpid-atom
                      ((10 20 30) (return-from landpoint-location
                                    (cons (+ x (* 5 (signum x))) (* lpid-atom (signum y)))))))
        ))
    ;; terminus

    (case state
      ((f g) nil) ;; normal: flag is behind us, so "see" just gives type
      (f-center (cons 0 0))
      (fx-on-off? (cons x y))
      (otherwise (error "invalid landpoint-location end state ~a" state)))))

(defun pt-in-penalty-area (p)
  (and (<= (abs *pa-top-ours*) (px p) (abs *goal-line-ours*))
    (<= (abs (py p)) (abs *pa-right*))))

(defmacro tll (&rest ids)
  `(landpoint-location ',ids))

#| test landpoint-location

(tll f c)
(tll f c t)
(tll f c b)
(tll g l)
(tll g r)
(tll f p l t)
(tll f p l c)
(tll f p l b)
(tll f p r t)
(tll f p r c)
(tll f p r b)
(tll f t 0) ;; version 9-friendly; in v7 they had these at the wrong place
(tll f g l t)
(tll f t l 10)
(tll f b l 10)
(tll f t r 10)
(tll f b r 10)
(tll f l t)
(tll f r t)
(tll f r b)
(tll f l t 10)
(tll f r t 10)
(tll f r b 30)

(vlp-test ((((F C) 10 0 0 0) 0 . 0) (((F P R B) 50.4 24) 36.0 . -20.16)))


|#

(defmacro vlp-test (((f1 . xy1)(f2 . xy2)))
  `(viewpoint-location (radians ,(caddr f1)) ,(cadr f1) ',xy1 
     (radians ,(caddr f2)) ,(cadr f2) ',xy2))



(defun test-vpl (x y los lp1 lp2)
  (let* ((xy (make-pt x y))
         (xy1 (landpoint-location lp1))
         (xy2 (landpoint-location lp2))
         (d1 (pt-dir-between xy xy1))
         (od1 (- d1 los))
         (s1 (pt-dist xy xy1))
         (d2 (pt-dir-between xy xy2))
         (od2 (- d2 los))
         (s2 (pt-dist xy xy2))
         )
    (VIEWPOINT-LOCATION od1 s1 xy1 od2 s2 xy2)))


#+test
(vlp-test ((((F C) 10 0 0 0) 0 . 0) (((F P R B) 50.4 24) 36.0 . -20.16)))

#+test
(let ((facing 45)(neck 0)(los 45)
        (x1 20)(y1 20)(x2 10)(y2 5)(x? 4)(y? 10))
    (declare (ignore neck los))
    (let (;(s3 (coord-dist x1 y1 x2 y2))
          ;
          (s1 (coord-dist x? y? x1 y1))
          (d1 (- (radians facing) (coord-dir-between x? y? x1 y1)))
          (s2 (coord-dist x? y? x2 y2))
          (d2 (- (radians facing) (coord-dir-between x? y? x2 y2)))
          )
      (let ((pos (viewpoint-location d1 s1 (cons x1 y1) d2 s2 (cons x2 y2))))
        (trc "dir pos xy1" (degrees d1) (degrees (coord-dir-between (car pos) (cdr pos) x1 y1)))
        (list pos (degrees (+ d1 (coord-dir-between (car pos) (cdr pos) x1 y1)))))
      ))



#+test
(pick-two-landpoints
 '((landpoints . (((F T R 10) 7.3 -16 0 0) ((F T R 20) 17.1 -7 0 0) ((F T R 30) 27.1 -4) 
                  ((F P R T) 37 27) ((F T R 40) 37 -3) ((F T R 50) 47 -2) ((F R T) 49.4 3) 
                  ((F R T 30) 55.1 7) ((F R T 20) 56.8 17) ((F G R T) 58 31) ((F R T 10) 60.9 26) 
                  ((G R) 61.6 37) ((F G R B) 66 42) ((F R 0) 66 34) ((F R B 10) 72.2 41)))))

#+test
(where-am-i? t
 '((landpoints . (((F T R 10) 7.3 -16 0 0) ((F T R 20) 17.1 -7 0 0) ((F T R 30) 27.1 -4) 
                  ((F P R T) 37 27) ((F T R 40) 37 -3) ((F T R 50) 47 -2) ((F R T) 49.4 3) 
                  ((F R T 30) 55.1 7) ((F R T 20) 56.8 17) ((F G R T) 58 31) ((F R T 10) 60.9 26) 
                  ((G R) 61.6 37) ((F G R B) 66 42) ((F R 0) 66 34) ((F R B 10) 72.2 41)))))


(defun test-vpls (steps x y lpid1 lpid2)
  (dotimes (s steps) ;; ok
    (let ((los (radians (* s (floor 360 steps)))))
      (print (cons (dgr los) (ptr (test-vpl x y los lpid1 lpid2)))))))

#|


(test-vpls 2  0 0 '(f p r t) '(f p r b))
(test-vpls 2 0 0 '(f g r t) '(f g r b))

(test-vpls 12 0 0 '(f b r 20) '(f b l 20))

(test-vpls 12 4 43 '(f t 0) '(f t r 10)) ; ok
(test-vpls 12 10 55 '(f c) '(g r)) ; ok
(test-vpls 12 0 0 '(f p r b) '(f b 0)) ; ok
(test-vpls 12 0 0 '(f p r c) '(f p r t)) ;ok
(test-vpls 12 0 0 '(f p r t) '(f p r c)) ;ok
(test-vpls 12 0 0 '(f p r t) '(f r b 20)) ;ok
(test-vpls 12 20 10 '(f p r c) '(f p r b)) ;ok
(test-vpls 12 -10 -5 '(f p r c) '(f c b))

(test-vpls 12 -10 -5 '(f p r c) '(f p r b))



(ptr (test-vpl 0 0 0 '(f g r t) '(f g r b)))

(progn
  (print (ptr (test-vpl 0 0 0 '(f p r t) '(f p r b))))
  (print (ptr (test-vpl 0 0 (radians 1) '(f p r t) '(f p r b))))
  (test-vpls 4 0 0 '(f b r 20) '(f b l 20))
  (values))

(progn
  (test-vpls 4 0 0 '(f t l 20) '(f t r 20))
  (test-vpls 4 0 0 '(f t r 20) '(f t l 20))
  (test-vpls 4 0 0 '(f b l 20) '(f b r 20))
  (test-vpls 4 0 0 '(f b r 20) '(f b l 20)))

|#