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

;____________________________ cell calculate and set ___________________
;

(defun c-calculate-and-set (c)
  (when *stop*
    (princ #\.)
    (return-from c-calculate-and-set))
  
  (count-it :c-calculate-and-set )
  ;;;  (count-it :c-calculate-and-set (type-of (c-model c))) ;; (c-slot-name c))

  ;;(with-metrics (nil nil () "calc n set" (c-slot-name c) (c-model c))
  (progn ;; wtrc (0 200 "calc n set" (c-slot-name c) (c-model c))
    (cd-usage-clear-all c)
    
    (let ((mycalc (incf (cr-rethinking c) 1))
          (newvalue (let ((*c-calculators* (cons c *c-calculators*))
                          *synapse-factory* ;; clear, then if desired each access to potential other cell must estab. *synapse-factory*
                          )
                      (assert (c-model c))
                      (funcall (cr-rule c) c))))

      #+notso (assert (not (typep newvalue 'cell)) ()
                "new value for cell ~s is itself a cell: ~s. probably nested (c? ... (c? ))"
                c newvalue)
      (when (and *c-debug* (typep newvalue 'cell))
        (trc "new value for cell ~s is itself a cell: ~s. probably nested (c? ... (c? ))"
                c newvalue))
      (when (< mycalc (cr-rethinking c))
        ;;
        ;; means we re-entered rule and managed to compute without re-entering under new circumstances
        ;; of later entry. use later calculation result..
        ;; (trc c "calc-n-set > breaking off, not lg" c)
        ;;
        (assert (c-validp c))
        (return-from c-calculate-and-set (c-value c)))
      
     (c-unlink-unused c)

     (md-slot-value-assume (c-model c)
                            (c-slot-spec c)
                            (c-absorb-value c newvalue)))))

(defun c-unlink-unused (c &aux (usage (cd-usage c)))
  (do ((useds (cd-useds c) (rest useds))
       (mapn (- *cd-usagect* (length (cd-useds c))) (1+ mapn)))
      ((null useds))
    (assert (not (minusp mapn)))
    (assert (< mapn *cd-usagect*))
    (when (zerop (sbit usage mapn))
      (let ((used (car useds)))
        (if (typep used 'synapse)
            (progn
              (setf (syn-relevant used) nil) ;; 030826synfix
              )
          (progn
            ;;(trc c "dropping unused" used :mapn-usage mapn usage)
            (c-unlink-user used c)
            (rplaca useds nil))))))
  (setf (cd-useds c) (delete-if #'null (cd-useds c))))


