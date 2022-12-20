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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(mksynapse fDelta fSensitivity fPlusp fZerop fDifferent)))

; ___________________________ cell relay value ___________________________________
        
(defparameter *relayspeak* nil)
(defmethod c-relay-value ((syn synapse) value)
   ;(trc "c-relay-value> syn, raw value:" syn value)
   (let ((res (funcall (syn-relay-value syn) syn value)))
      ;(trc "c-relay-value> cell, filtered value:" syn res)
      res))

(defmethod c-relay-value (cell value)
   (declare (ignorable cell))
   (when *relayspeak*
      (trc "c-relay-value unspecial > cell value" cell value)
      (setf *relayspeak* nil))
   value)

;__________________________________________________________________________________
;
(defmethod delta-diff ((new number) (old number) subtypename)
  (declare (ignore subtypename))
  (- new old))

(defmethod delta-identity ((dispatcher number) subtypename)
  (declare (ignore subtypename))
  0)

(defmethod delta-abs ((n number) subtypename)
  (declare (ignore subtypename))
  (abs n))

(defmethod delta-exceeds ((d1 number) (d2 number) subtypename)
  (declare (ignore subtypename))
  (> d1 d2))

(defmethod delta-greater-or-equal ((d1 number) (d2 number) subtypename)
  (declare (ignore subtypename))
  (>= d1 d2))

;_________________________________________________________________________________
;
(defmethod delta-diff (new old (subtypename (eql 'boolean)))
   (if new
       (if old
           :unchanged
         :on)
     (if old
         :off
       :unchanged)))


(defmethod delta-identity (dispatcher (subtypename (eql 'boolean)))
   (declare (ignore dispatcher))
   :unchanged)

;______________________________________________________________

(defun fdeltalist (&key (test #'true))
  (mksynapse (priorlist)
             :fire-p (lambda (syn newlist)
                           (declare (ignorable syn))
                           (or (find-if (lambda (new)
                                            ;--- gaining one? ----
                                            (and (not (member new priorlist))
                                                 (funcall test new)))
                                        newlist)
                               (find-if (lambda (old)
                                            ;--- losing one? ----
                                            (not (member old newlist))) ;; all olds have passed test, so skip test here
                                        priorlist)))
             
             :relay-value (lambda (syn newlist)
                                (declare (ignorable syn))
                                ;/// excess consing on long lists
                                (setf priorlist (remove-if-not test newlist)))))

;_______________________________________________________________

(defun ffindonce (finderfn)
  (mksynapse (bingo bingobound)

             :fire-p (lambda (syn newlist)
                            (declare (ignorable syn))
                            (unless bingo ;; once found, yer done
                              (setf bingobound t
                                bingo (find-if finderfn newlist))))

             :relay-value (lambda (syn newlist)
                                (declare (ignorable syn))
                                (or bingo
                                    (and (not bingobound) ;; don't bother if fire? already looked
                                         (find-if finderfn newlist))))))
                                
;___________________________________________________________________             

(defun fsensitivity (sensitivity &optional subtypename)
  (mksynapse (priorrelayvalue)
    :fire-p (lambda (syn newvalue)
              (declare (ignorable syn))
              (eko (nil "fire-p decides" priorrelayvalue sensitivity)
                (delta-greater-or-equal
                 (delta-abs (delta-diff newvalue priorrelayvalue subtypename) subtypename)
                 (delta-abs sensitivity subtypename) 
                 subtypename)))
    
    :relay-value (lambda (syn newvalue)
                   (declare (ignorable syn))
                   (eko (nil "fsensitivity relays")
                     (setf priorrelayvalue newvalue)) ;; no modulation of value, but do record for next time
                   )))

(defun fPlusp ()
  (mksynapse (priorrelayvalue)
    :fire-p (lambda (syn new-basis)
              (declare (ignorable syn))
              (eko (nil "fPlusp fire-p decides" priorrelayvalue sensitivity)
                (xor priorrelayvalue (plusp new-basis))))
    
    :relay-value (lambda (syn new-basis)
                   (declare (ignorable syn))
                   (eko (nil "fPlusp relays")
                     (setf priorrelayvalue (plusp new-basis))) ;; no modulation of value, but do record for next time
                   )))

(defun fZerop ()
  (mksynapse (priorrelayvalue)
    :fire-p (lambda (syn new-basis)
              (declare (ignorable syn))
              (eko (nil "fZerop fire-p decides")
                (xor priorrelayvalue (zerop new-basis))))
    
    :relay-value (lambda (syn new-basis)
                   (declare (ignorable syn))
                   (eko (nil "fZerop relays")
                     (setf priorrelayvalue (zerop new-basis)))
                   )))

(defun fDifferent ()
  (mksynapse (prior-object)
    :fire-p (lambda (syn new-object)
              (declare (ignorable syn))
              (trc nil "fDiff: prior,new" (not (eql new-object prior-object))
                prior-object new-object)
              (not (eql new-object prior-object)))
    
    :relay-value (lambda (syn new-object)
                   (declare (ignorable syn))
                   (unless (eql new-object prior-object)
                     (setf prior-object new-object)))
    ))
;
;____________________ synapse constructors _______________________________
;
(defun fdelta (&key sensitivity (type 'number))
  (mksynapse (lastrelaybasis lastboundp)
              :fire-p (lambda (syn newbasis)
                            (declare (ignorable syn))
                            (eko (nil "delta fire-p")
                                 (or (null sensitivity)
                                     (let ((delta (delta-diff newbasis lastrelaybasis type)))
                                       (delta-exceeds delta sensitivity type)))))

              :relay-value (lambda (syn newbasis)
                                 (declare (ignorable syn))
                                 (prog1
                                     (if lastboundp
                                         (delta-diff newbasis lastrelaybasis type)
                                       (delta-identity newbasis type))
                                   ;(trc "filter yields to user, value" (c-slot-name user) (c-slot-spec syn) relayvalue)
                                   ;(trc "fdelta > ********************* new lastrelay! " syn lastrelaybasis)
                                   (setf lastboundp t)
                                   (setf lastrelaybasis newbasis)))
              ))



(defmethod delta-exceeds (booldelta sensitivity (subtypename (eql 'boolean)))
  (unless (eql booldelta :unchanged)
    (or (eq sensitivity t)
        (eq sensitivity booldelta))))

(defun fboolean (&optional (sensitivity 't))
  (fdelta :sensitivity sensitivity :type 'boolean))
        

#| ---- hunh? ----

#+test

(delta-diff nil nil 'boolean)

#+test
(defclass nz ()
   ((numz :cell t :accessor numz
          :initform (cv 0)
          :cellecho ((self newvalue oldvalue)
                   (declare (ignorable self oldvalue))
                   (trc "numz <- " newvalue)))
    (testz :cell t :reader testz
           :initform (c? ;;(trc "rethinking testz" *cause*)
                          (^numz self (fdelta :sensitivity 5)))
           :cellecho ((self newvalue oldvalue)
                    (declare (ignorable self oldvalue))
                    (trc "testz <- " newvalue))
           )))

#+test

(let ((n (md-make 'nz)))
   (dotimes (x 20)
     (setf (numz n) (- 10 (random 20)))))

#+test

(let ((xx (md-make 'xxx)))
   (dotimes (x 5)
     (push x (aaa xx)))
   (dotimes (x 5)
     (setf (aaa xx) (cdr (aaa xx))))
   )

;;; (defclass xxx ()
;;;    ((aaa :cell t :initform (cv nil) :accessor aaa)
;;;     (bbb :cell t :initform (c? (trc "bbb fires>>>>>>>>>>")
;;;                                     (^aaa self (fdeltalist :test #'evenp))) :reader bbb)))

;;; (test-echo bbb :when nil)

|#
