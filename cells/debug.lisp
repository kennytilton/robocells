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

(defun cellstop ()
  ;; (break "in-cell-stop")
  (setf *stop* t))

(defun cellbrk (&optional (tag :anon))
  (unless (or *stop*)
    ;; daring move, hoping having handler at outside stops the game (cellstop)
    (print `(cell break , tag))
    (break)))

;----------- trc -------------------------------------------

(defun trcdepth-reset ()
  (setf *trcdepth* 0))

(defmacro trc (tgtform &rest os)
  (unless (eql tgtform 'nil)
    (if (stringp tgtform)
        `(call-trc ,tgtform ,@os)
      (let ((tgt (gensym)))
        `(without-c-dependency
          (bif (,tgt ,tgtform)
            (if (trcp ,tgt)
                (progn
                  (assert (stringp ,(car os)))
                  (call-trc ,@os)) ;;,(car os) ,tgt ,@(cdr os)))
              (count-it :trcfailed))
            (count-it :tgtnileval)))))))

(defun call-trc (s &rest os)
  (if #+cormanlisp nil #-cormanlisp (and (boundp '*trcdepth*)
          *trcdepth*)
        (format t "~&~v,,,'.<~d~>> " (mod *trcdepth* 100) *trcdepth*)
      (format t "~&"))
    
    (format t "~a" s)
    (dolist (o os) (format t " ~s" o))
    (values))
  
#+findtrcevalnils
(defmethod trcp :around (other)
  (unless (call-next-method other)(break)))

(defmethod trcp (other)
  (eq other t))
  
(defmethod trcp (($ string))
  t)
  
(defun trcdepth-incf ()
  (incf *trcdepth*))
  
(defun trcdepth-decf ()
  (format t "decrementing trc depth" *trcdepth*)
  (decf *trcdepth*))
  
(defmacro wtrc ((&optional (min 1) (max 50) &rest banner) &body body )
  `(let ((*trcdepth* (if *trcdepth*
                         (1+ *trcdepth*)
                       0)))
     ,(when banner `(when (>= *trcdepth* ,min)
                      (if (< *trcdepth* ,max)
                          (trc ,@banner)
                        (progn
                          (break "excess trace notttt!!! ~d" *trcdepth*) ;; ,@banner)
                          nil))))
     (when (< *trcdepth* ,max)
       ,@body)))

(defmacro wnotrc ((&optional (min 1) (max 50) &rest banner) &body body )
  (declare (ignore min max banner))
  `(progn ,@body))
  
;------ eko --------------------------------------


(defmacro eko ((&rest trcargs) &rest body)
  (let ((result (gensym)))
     `(let ((,result ,@body))
         (trc ,(car trcargs) ,(cadr trcargs) :result ,result ,@(cddr trcargs))
         ,result)))

(defmacro ek (label &rest body)
  (let ((result (gensym)))
     `(let ((,result (,@body)))
         (when ,label
           (trc ,label ,result))
         ,result)))

;------------- counting ---------------------------
(defvar *count* nil)
(defvar *counting* nil)

(defmacro with-counts ((onp &rest msg) &body body)
  `(if ,onp
      (prog2
         (progn
           (count-clear ,@msg)
           (push t *counting*))
           (progn ,@body)
         (pop *counting*)
         (show-count t ,@msg))
    (progn ,@body)))
  
(defun count-clear (&rest msg)
  (declare (ignorable msg))
  (format t "~&count-clear > ~a" msg)
  (setf *count* nil))

(defmacro count-it (&rest keys)
  `(when *counting*
     (call-count-it ,@keys)))

(defun call-count-it (&rest keys)
    (declare (ignorable keys))
  ;;; (when (eql :TGTNILEVAL (car keys))(break))
  (let ((entry (assoc keys *count* :test #'equal)))
      (if entry
          (setf (cdr entry) (1+ (cdr entry)))
        (push (cons keys 1) *count*))))

(defun show-count (clearp &rest msg)
  (format t "~&Counts after: clearp ~a, length ~d: ~s" clearp (length *count*) msg)
  (let ((res (sort (copy-list *count*) (lambda (v1 v2)
                                           (let ((v1$ (symbol-name (caar v1)))
                                                 (v2$ (symbol-name (caar v2))))
                                             (if (string= v1$ v2$)
                                                 (< (cdr v1) (cdr v2))
                                               (string< v1$ v2$))))))
        (running 0))
     (dolist (entry res)
       (when (> (cdr entry) 0)
         (incf running (cdr entry))
         (format t "~&~4d ... ~2d ... ~s" running (cdr entry) (car entry)))))
  (when clearp (count-clear "show-count")))
  

;-------------------- timex ---------------------------------

;;;(defmacro timex ((onp &rest trcArgs) &body body)
;;;  `(if ,onp
;;;       (prog1
;;;           (time
;;;            (progn ,@body))
;;;         (trc "timing was of" ,@trcARgs))
;;;     (progn ,@body)))


;---------------- Metrics -------------------

(defmacro with-metrics ((countp timep &rest trcargs) &body body)
  `(with-counts (,countp ,@trcargs)
    (timex (,timep ,@trcargs)
       ,@body)))


; -------- cell conditions (not much used) ---------------------------------------------

(define-condition xcell () ;; new 2k0227
  ((cell :initarg :cell :reader cell :initform nil)
   (appfunc :initarg :appfunc :reader appfunc :initform 'badcell)
   (errortext :initarg :errortext :reader errortext :initform "<???>")
   (otherdata :initarg :otherdata :reader otherdata :initform "<nootherdata>"))
  (:report (lambda (c s)
             (format s "~& trouble with cell ~a in function ~s,~s: ~s"
               (cell c) (appfunc c) (errortext c) (otherdata c)))))

(define-condition c-enabling ()
   ((name :initarg :name :reader name)
    (model :initarg :model :reader model)
    (cell :initarg :cell :reader cell))
   (:report (lambda (condition stream)
                 (format stream "~&unhandled <c-enabling>: ~s" condition)
                 (break "~&i say, unhandled <c-enabling>: ~s" condition))))

(define-condition c-fatal (xcell)
   ((name :initarg :name :reader name)
    (model :initarg :model :reader model)
    (cell :initarg :cell :reader cell))
   (:report (lambda (condition stream)
              (format stream "~&fatal cell programming error: ~s" condition)
              (format stream "~&  : ~s" (name condition))
              (format stream "~&  : ~s" (model condition))
              (format stream "~&  : ~s" (cell condition)))))

(define-condition c-unadopted (c-fatal)
   ()
   (:report
    (lambda (condition stream)
      (format stream "~&unadopted cell >: ~s" (cell condition))
      (format stream "~& >: often you mis-edit (c? (c? ...)) nesting is error"))))


;----------------------------- link debugging -----------------------


(defun dump-users (c &optional (depth 0))
     (format t "~&~v,4t~s" depth c)
     (dolist (user (un-users c))
          (dump-users user (+ 1 depth))))

(defun dump-useds (c &optional (depth 0))
     ;(c.trc "dump-useds> entry " c (+ 1 depth))
     (when (zerop depth)
          (format t "x~&"))
     (format t "~&|usd> ~v,8t~s" depth c)
     (when (typep c 'c-ruled)
          ;(c.trc "its ruled" c)
          (dolist (used (cd-useds c))
               (dump-useds used (+ 1 depth)))))


(defun cell-reset ()
  (setf *count* nil
    *stop* nil
    *dbg* nil
    *mybreak* nil
    *c-prop-depth* 0
    *sw-looping* nil
    *to-be-awakened* nil
    *trcdepth* 0))

