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
  (export '(family-values family-values-sorted
            sortindex sortdirection sortpredicate sortkey
            ^sortindex ^sortdirection ^sortpredicate ^sortkey)))

(defmodel family-values (family)
  (
   (kvcollector :initarg :kvcollector
                :initform #'identity
                :reader kvcollector)
   
   (kidvalues :cell t
     :initform (c? (when (kvcollector self)
                       (funcall (kvcollector self) (^mdvalue))))
     :accessor kidvalues
     :initarg :kidvalues)
   
   (kvkey :initform #'identity
          :initarg :kvkey
          :reader kvkey)
   
   (kvkeytest :initform #'equal
              :initarg :kvkeytest
              :reader kvkeytest)
   
   (kidfactory :cell t
               :initform #'identity
               :initarg :kidfactory
               :reader kidfactory)
   
   (.kids :cell t
     :initform (c? (assert (listp (kidvalues self)))
                 (let ((newkids (mapcan (lambda (kidvalue)
                                               (list (or (find kidvalue (c-value c)
							       :key (kvkey self)
							       :test (kvkeytest self))
                                                         (trc nil "family-values forced to make new kid" 
							      self (c-value c) kidvalue)
                                                  (funcall (kidfactory self) self kidvalue))))
                                  (^kidvalues))))
                   (nconc (mapcan (lambda (oldkid)
                                    (unless (find oldkid newkids)
                                      (when (fv-kid-keep self oldkid)
                                        (list oldkid))))
                            (c-value c))
                     newkids)))
     :accessor kids
     :initarg :kids)))

(defmethod fv-kid-keep (family oldkid)
  (declare (ignorable family oldkid))
  nil)

(defmodel family-values-sorted (family-values)
  ((sortedkids :cell t :initarg :sortedkids :accessor sortedkids
     :initform nil)
   (sortmap :cell t :initform (cv nil) :initarg :sortmap :accessor sortmap)
   (.kid-slots :cell t
     :initform (c? (assert (listp (kidvalues self)))
                 (mapsort (^sortmap)
                   (thekids
                    (mapcar (lambda (kidvalue)
                              (trc "making kid" kidvalue)
                              (or (find kidvalue (c-value c) :key (kvkey self) :test (kvkeytest self))
                                (trc nil "family-values forced to make new kid" self .cache. kidvalue)
                                (funcall (kidfactory self) self kidvalue)))
                      (^kidvalues)))))
     :accessor kid-slots
     :initarg :kid-slots)))

(defun mapsort (map data)
  ;;(trc "mapsort map" map)
  (if map
      (stable-sort data #'< :key (lambda (datum) (or (position datum map)
                                                       ;(trc "mapsort datum not in map" datum)
                                                       (1+ (length data)))))
    data))

(def-c-echo sortedkids ()
  (setf (sortmap self) new-value)) ;; cellular trick to avoid cyclicity

