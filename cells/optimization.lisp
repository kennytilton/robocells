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

(in-package :cells)

;____________ cell when ____________________

(defparameter *c-whentime* nil)

(defun call-with-when-time? (whentimes function &aux old)
  (rotatef old *c-whentime* whentimes)
  ;; (trc "setting *c-whentime* to" *c-whentime*)
  (unwind-protect
      (funcall function)
    (setf *c-whentime* old)))

;---------- optimizing away cells whose dependents all turn out to be constant ----------------
;

(defun c-optimize-away?! (c)
  
  (typecase c
    #+old-code
    (c-nested (trc nil "optimize-away nested")
              (when (and (null (cd-useds c)))
                (rplaca (member c (cellnestedcells (cellaggregatecell c))) (c-value c))
                t))
    (c-dependent
     (if (and *c-optimizep*
              (c-validp c)
              (null (cd-useds c)))
         
         (progn
           (trc nil "optimizing away" c)
           (count-it :c-optimized)
           
           (setf (c-state c) :optimized-away)

           (let ((entry (rassoc c (cells (c-model c))))) ; move from cells to cells-flushed
             (assert entry)
             (setf (cells (c-model c)) (delete entry (cells (c-model c))))
             (push entry (cells-flushed (c-model c))))
           
           (dolist (user (un-users c))
             (setf (cd-useds user) (delete c (cd-useds user)))
             (when (c-optimize-away?! c)
               (break  "just checking this ever happens: optimizing chain reaction" c)))
           
           (setf ; drop foreign refs to aid gc (gc paranoia?)
            (c-model c) nil
            (un-users c) nil)

           t)
       
       (progn
         (trc nil "not optimizing away" *c-optimizep* (car (cd-useds c)) (c-validp c))
         #+no (dolist (used (cd-useds c))
                (assert (member c (un-users used)))
                ;;; (trc c "found as user of" used)
                )
         ;  (count-it :c-not-optimize)
         ;  (count-it (intern-keyword "noopti-" #+nah (c-model c) "-" (symbol-name (c-slot-name c))))
         )))))
