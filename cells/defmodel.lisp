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

(defmacro defmodel (class directsupers slotspecs &rest options)
  ;;(print `(defmodel sees directsupers ,directsupers using ,(or directsupers :model-object)))
  `(progn
     (eval-when (:compile-toplevel :execute :load-toplevel)
       (setf (get ',class :cell-defs) nil))
     ;
     ; define slot macros before class so they can appear in initforms and default-initargs
     ;
     ,@(mapcar (lambda (slotspec)
                 (destructuring-bind
                     (slotname &rest slotargs
                       &key cell accessor reader
                       &allow-other-keys)
                     slotspec
                   (declare (ignorable slotargs))
                   (when cell
                     (let* ((readerfn (or reader accessor))
                            (deriverfn (intern$ "^" (symbol-name readerfn)))
                            )
                       ;
                       ; may as well do this here...
                       ;
                       ;;(trc nil "slot, deriverfn would be" slotname deriverfn)
                       `(eval-when (:compile-toplevel :execute :load-toplevel)
                          (setf (md-slot-cell-type ',class ',slotname) ,cell)
                          (unless (macro-function ',deriverfn)
                            (defmacro ,deriverfn (&optional (model 'self) synfactory)
                              `(let ((*synapse-factory* ,synfactory))
                                 (,',readerfn ,model))))
                          )
                       ))
                   ))
         slotspecs)
     
     ;
     ; -------  defclass ---------------  (^slot-value ,model ',',slotname)
     ;
     
     (defclass ,class ,(or directsupers '(model-object));; now we can def the class
       ,(mapcar (lambda (s)
                  (list* (car s)
                    (let ((ias (cdr s)))
                      (remf ias :cell)
                      (remf ias :cwhen)
                      ias))) (mapcar #'copy-list slotspecs))
       (:documentation
        ,@(or (cdr (find :documentation options :key #'car))
            '("chya")))
       (:default-initargs ;; nil ok and needed: acl oddity in re not clearing d-i's sans this
           ,@(cdr (find :default-initargs options :key #'car)))
       (:metaclass ,(or (find :metaclass options :key #'car)
                      'standard-class)))
     
     (defmethod shared-initialize :after ((self ,class) slot-names &rest iargs)
       (declare (ignore slot-names iargs))
       (unless (typep self 'model-object)
         (error "If no superclass of ~a inherits directly
or indirectly from model-object, model-object must be included as a direct super-class in
the defmodel form for ~a" ',class ',class)))
     ;
     ; slot accessors once class is defined...
     ;
     ,@(mapcar (lambda (slotspec)
                 (destructuring-bind
                     (slotname &rest slotargs
                       &key cell accessor reader writer type
                       &allow-other-keys)
                     slotspec
                   (declare (ignorable slotargs))
                   (when cell
                     (let* ((readerfn (or reader accessor))
                            (writerfn (or writer accessor))
                            )
                       (setf (md-slot-cell-type class slotname) cell)
                       
                       `(progn
                          ,(when readerfn
                             `(defmethod ,readerfn ((self ,class))
                                (md-slot-value self ',slotname)))
                          
                          ,(when writerfn
                             `(defmethod (setf ,writerfn) (new-value (self ,class))
                                (setf (md-slot-value self ',slotname)
                                  ,(if type
                                      `(coerce new-value ',type)
                                    'new-value))))
                          )
                       ))
                   ))
         slotspecs)))