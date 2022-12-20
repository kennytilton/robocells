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

(defun see (sights)
  (flet ((a-sort-by-dist (assoc)
           (dolist (entry assoc assoc)
             (unless (eql 'ball (car entry))
               (setf (cdr entry) (sort (cdr entry) #'< :key #'second))))))
    ;; (trc "see> sights" sights)
    (macrolet ((sight-see (type) `(gather ',type sights :key #'caar)))
      (let* ((players (sight-see p))
             (teams (delete-duplicates (mapcar #'cadar players)))
             (rosters (mapcar (lambda (team)
                                (gather team players :key #'cadar))
                        teams)))
        (a-sort-by-dist
         (list*
          (cons 'flags (sight-see f))
          (cons 'landpoints (remove-if-not
                             (lambda (s)
                               (find (caar s) '(f g)))
                             sights))
          (cons 'ball (find 'b sights :key #'caar))
          (cons 'goals (sight-see g))
          (cons 'lines (sight-see l))
          (mapcar (lambda (team roster)
                    (cons team roster))
            teams rosters)))))))

(defun seen-dir (sight)
  (if (eql 2 (length sight))
    (second sight)
    (third sight)))

(defun seen-dist (sight)
  (unless (eql 2 (length sight))
    (second sight)))

(defun seen-ddist (sight)
  (elt sight 3))

(defun seen-ddir (sight)
  (elt sight 4))

#| 

evaluate:

(chya!
  "(see 0 ((f c) 10.5 12 0 0) ((f r t) 73 -2) ((f g r b) 62.8 32) ((g r) 62.8 26) 
((f g r t) 63.4 19) ((f p r c) 46.1 25) ((f p r t) 51.4 2) ((f t r 10) 46.5 -36) 
((f t r 20) 51.4 -26) ((f t r 30) 58 -18) ((f t r 40) 65.4 -12) ((f t r 50) 73 -7) 
((f r 0) 67.4 26) ((f r t 10) 68.7 18) ((f r t 20) 71.5 10) ((f r t 30) 75.2 2) 
((f r b 10) 68 34) ((f r b 20) 69.4 43) ((b) 10 12 0 0) ((p \"TeamKenny\" 1) 9 10 0 0) 
((p \"EmpireEvil\" 1) 24.5 -5 0 0) ((p \"EmpireEvil\" 1) 24.5 -15 0 0) ((l r) 70.8 -62))"
 )


|#

;--- sense body access -----------------------------------

(defmacro p-sensed-info (which)
  `(rc-item (cdr (sensed self)) ',which))

(define-symbol-macro .head-angle.
    (p-sensed-info head_angle))

(define-symbol-macro .p-speed.
    (p-sensed-info speed-amt))

(define-symbol-macro .p-speed-dir.
    (p-sensed-info speed-dir))

(define-symbol-macro .neck.
    (p-sensed-info head_angle))
      
