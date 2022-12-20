;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
;;; 
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

(defmacro maxf (place &rest othervalues)
   `(setf ,place (max ,place ,@othervalues)))

(defun last1 (thing)
     (car (last thing)))

(defun max-if (&rest values)
  (let (max)
    (dolist (value values max)
      (when value
        (setq max (if max (max max value) value))))))

(defun min-max-of (v1 v2)
     (values (min-if v1 v2) (max-if v1 v2)))

(defun min-if (v1 v2)
     (if v1 (if v2 (min v1 v2) v1) v2))

(defun list-flatten! (&rest list)
  (if (consp list)
    (let (head work visited)
      (labels ((link (cell)
                 ;;(format t "~&Link > cons: ~s . ~s" (car cell) (cdr cell))
                 (when (and (consp cell)
                            (member cell visited))
                   (break "list-flatten! detects infinite list: cell ~a, visited ~a" cell visited))
                 (push cell visited)
                 
                 (when cell
                   (if (consp (car cell))
                      (link (car cell))
                      (progn
                       (setf head (or head cell))
                       (when work
                          (rplacd work cell))
                       (setf work cell)))
                   (link (rest cell)))))
        (link list))
      head)
    list))

(defun packed-flat! (&rest uNameit)
   (delete-if #'null (list-flatten! uNameIt)))

(defmacro with-dynamic-fn ((fnName (&rest fnArgs) &body fnBody) &body body)
  `(let ((,fnName (lambda ,fnArgs ,@fnBody)))
     (declare (dynamic-extent ,fnname))
     ,@body))

(eval-when (compile load eval)
  (export 'myAssert))

(defmacro myAssert (assertion &optional places fmt$ &rest fmtargs)
  (declare (ignore places))
    
  `(unless *stop*
     (unless ,assertion
       (setf *stop* t)
       ,(if fmt$
            `(mybreak ,fmt$ ,@fmtargs)
          `(mybreak "failed assertion:" ',assertion)))))

(defvar *mybreak*)

(defun mybreak (&rest args)
  (unless (or *mybreak* *stop*)
    (setf *mybreak* t)
    (setf *stop* t)
    (format t "mybreak > stopping > ~a" args)
    (apply #'break args)))

(defun assocv (sym assoc)
  (cdr (assoc sym assoc)))

(defmacro assocv-setf (assoc-place sym-form v)
  (let ((sym (gensym))(entry (gensym)))
    `(let ((,sym ,sym-form))
       (bIf (,entry (assoc ,sym ,assoc-place))
            (rplacd ,entry ,v)
            (push (cons ,sym ,v) ,assoc-place)))))

(defun intern$ (&rest strings)
  (intern  (apply #'concatenate 'string (mapcar #'string-upcase strings))))

#-allegro
(defmacro until (test &body body)
  `(LOOP (WHEN ,test (RETURN)) ,@body))

#-allegro
(defmacro while (test &body body)
  `(LOOP (unless ,test (RETURN)) ,@body))

(defmacro bwhen ((bindvar boundform) &body body)
  `(let ((,bindvar ,boundform))
      (declare (ignorable ,bindvar))
      (when ,bindvar
        ,@body)))
  
(defmacro bif ((bindvar boundform) yup &optional nope)
  `(let ((,bindvar ,boundform))
      (if ,bindvar
         ,yup
         ,nope)))

(defmacro maptimes ((nvar count) &body body)
  (let ((result (gensym)))
     `(let (,result)
         (dotimes (,nvar ,count (nreverse ,result))
           (push (progn ,@body) ,result)))))

; --- cloucell support for struct access of slots ------------------------

(eval-when (:compile-toplevel :execute :load-toplevel)
  (export '(cc-defstruct instance-slots)))

(defmacro cc-defstruct (header &rest slots)
  (let (name concname (cache (gensym)))
    (if (consp header)
        (destructuring-bind (hname &rest options)
            header
          (setf name hname)
          (setf concname (bIf (concoption (find :conc-name options :key #'car))
                           (unless (eql (second concoption) 'nil)
                             (second concoption))
                           (intern (concatenate 'string
                               (symbol-name hname)
                               "-")))))
      (progn
        (setf name header)
        (setf concname (intern (concatenate 'string
                               (symbol-name header) "-")))))

    (let ((cc-info (mapcar (lambda (s)
                              (let ((sn (if (consp s)
                                            (car s) s)))
                                (cons sn
                                  (intern (concatenate 'string
                                            (when concname (symbol-name concname))
                                            (symbol-name sn))))))
                      slots)))
    `(progn
       (defstruct ,header ,@slots)
       (let (,cache)
         (defmethod instance-slots ((self ,name))
           (or ,cache (setf ,cache (append (call-next-method) ',cc-info)))))
       ))))

(defmethod instance-slots (root)
  (declare (ignorable root)))

