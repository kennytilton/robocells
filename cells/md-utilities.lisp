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

;;;(defmethod update-instance-for-redefined-class ((self model-object) added lost plist &key)
;;;  (declare (ignorable added lost plist))
;;;  (when (slot-boundp self '.md-state) (call-next-method)))

(defmethod occurence ((self model-object))
  ;
  ; whether multiply occuring or not, return index of self
  ; within list of likenamed siblings, perhaps mixed amongst others
  ; of diff names
  ;
  (let ((selfindex -1))
     (dolist (kid (kids (fmparent self)))
       (when (eql (md-name kid) (md-name self))
         (incf selfindex)
         (when (eql self kid)
           (return-from occurence selfindex))))))


(defun md-awake (self) (eql :awake (md-state self)))


(defun fm-grandparent (md)
  (fmparent (fmparent md)))


(defmethod md-release (other)
  (declare (ignorable other)))

;___________________ birth / death__________________________________
  
(defmethod not-to-be :around (self)
  (call-next-method)
  (trc nil "not-to-be clearing fmparent" self)
  (setf (fmparent self) nil
    (md-state self) :eternal-rest))

(defmethod not-to-be ((self model-object))
  (unless (md-untouchable self)
    (md-quiesce self)))

(defmethod md-untouchable (self) ;; would be t for closed-stream under acl
  (declare (ignore self))
  nil)

(defun md-quiesce (self)
  (md-map-cells self nil #'(lambda (cell)
                                 (c-quiesce cell))))


(defmethod not-to-be (other)
  other)



(defparameter *to-be-dbg* nil)

(defun to-be (self)
  (trc nil "to-be> entry" self (md-state self))

  (progn ;;wtrc (0 100 "to-be> entry" self (md-state self) (length *to-be-awakened*))
    (when (eql :nascent (md-state self)) ;; formwithview to-be-primary :after => rv-stitch! => side-effects
      (let ((already *to-be-awakened*))
        (setf *to-be-awakened* (nconc *to-be-awakened* (list self)))
        (trc nil "to-be deferring awaken" self)
        (kids self) ;; sick, just for side effect
        (unless already
          (trc nil "top to-be awakening deferred" self (length *to-be-awakened*))
          (do* ((mds *to-be-awakened* (cdr mds))
                (md (car mds) (car mds)))
               ((null mds))
            (if (eql :nascent (md-state md))
                (md-awaken md)
              (trc nil "not md-awakening non-nascent" md)))
          (setf *to-be-awakened* nil)))))
  self)

(defun md-make (class &rest kwps)
  (to-be (apply #'make-instance class kwps)))

