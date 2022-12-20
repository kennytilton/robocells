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

(defparameter *df-interference-detection* t)

(eval-when (compile eval load)
  (export '(*df-interference-detection*)))

(defmethod sw-detect-interference (user trigger)
  (declare (ignorable trigger))
  (when #+runtime-system t #-runtime-system *df-interference-detection*
    (trc nil "detect entry" user (cd-useds user))
    (dolist (used (cd-useds user))
      (bwhen (deepstalep (cd-stale-p-deep used))
        (trc nil "!! true deep stalep: user>" user)
        (trc nil "!! true deep stalep: used>" used)
        (trc nil "!! true deep stalep: deepstale>" deepstalep)
        (return-from sw-detect-interference deepstalep #+debugging (list user used deepstalep))))))

(defmethod sw-detect-interference ((user c-variable) trigger)
  (declare (ignore trigger)))

(defmethod sw-detect-interference ((user synapse) trigger)
  (sw-detect-interference (syn-used user) trigger))


(defmethod cd-stale-p-deep ((c c-dependent))
  (if (cd-stale-p c)
      c ;; not deep, shallow stalep, ok for our purposes
    (some #'cd-stale-p-deep (cd-useds c))))

#+debugversion
(defmethod cd-stale-p-deep ((c c-dependent))
  (if (cd-stale-p c)
      (eko ("deep stalep bingo !!!!!!")
           c) ;; not deep, shallow stalep, ok for our purposes
    (some #'cd-stale-p-deep (cd-useds c))))

(defmethod cd-stale-p-deep ((syn synapse))
  (cd-stale-p-deep (syn-used syn)))

(defmethod cd-stale-p-deep (c)
  (declare (ignore c)))

(defparameter *sw-pending* nil)
(defparameter *dataflowing* nil)

(defun dump-pending ()
  (dotimes (x (length *sw-pending*))
    (let ((p (nth x *sw-pending*)))
      (destructuring-bind (heldup . holdup) p
        (declare (ignorable holdup))
        (trc heldup "        pending!!!!!!!!!!" p)))))

;; mo better diags: holdup (c-true-stalep holdup) heldup (c-true-stalep heldup))))))

(defun call-with-dataflow-management (c-originating bodyfn)
  (declare (ignorable c-originating))
  (if *dataflowing*
      (funcall bodyfn)
    (let ((*dataflowing* t)
          *sw-pending*)
      #+dfdbg (trc nil ">>>>> with-dataflow-management: 001" c-originating)
      (setf (unst-setting-p c-originating) t)
      (prog1
          (funcall bodyfn)
        
        (while (and *sw-pending*
                 (not *sw-looping*))
          
          #+dfdbg
          (progn
            (trc nil "we have pending!!!!!!!!!!" (length *sw-pending*))
            (dump-pending))
          
          (let ((pct (length *sw-pending*))
                (oldpending (copy-list *sw-pending*)))
            ;;(trace c-rethink)
            (labels ((do-last (pending)
			(when pending
			  (do-last (cdr pending))
			  ;;(trace c-rethink cd-stale-p-deep sw-detect-interference)
			  (destructuring-bind (heldup . holdup) (car pending)
			    ;; (trc "pending sweep sees held up" heldup :holdup holdup)
			    (assert (find heldup (cells (c-model heldup)) :key #'cdr))
			    (assert (find holdup (cells (c-model holdup)) :key #'cdr))
			    ;; (unless (c-true-stalep holdup)
			    ;;     (trc nil "dataflow sees freed blocker" holdup))
			    (when (c-true-stalep holdup)
			      ;; (trc "dataflow retrying blocker" holdup)
			      (c-rethink holdup nil))
			    ;;(unless (c-true-stalep heldup)
				;;     (trc nil "dataflow sees freed blocked" heldup))
			    (when (c-true-stalep heldup)
			      ;; (trc "dataflow retrying blocked" heldup)
			      (c-rethink heldup nil))))))
              ;; (trace c-rethink cd-stale-p-deep sw-detect-interference)
              (do-last *sw-pending*)
              ;; (trc "post sweep pending leftovers:" (length *sw-pending*))
              ;; (untrace c-rethink cd-stale-p-deep sw-detect-interference)
              )
            ;;(untrace c-rethink)
            (when (and (equal oldpending *sw-pending*)
                    (eql pct (length *sw-pending*))
                    (not *sw-looping*))
              (setf *sw-looping* t)
              ;; (trc "interference 003" #+nooo (sw-detect-interference (car *sw-pending*) nil))
              ;; (wkill)
              #+nah (dolist (p *sw-pending*)
                      (destructuring-bind (heldup . holdup) p
                        (dump-dependency-path holdup heldup)))
              #+nah (dolist (p *sw-pending*)
                      (destructuring-bind (heldup . holdup) p
                        (declare (ignorable heldup))
                        (when t ;; (trcp holdup)
                          (dump-stale-path holdup))))
              (break "trigger ~a stuck; cant lose pendings ~a"
                c-originating
                *sw-pending*))
            
            ;; (trc "after sweep sw-pending" *sw-pending*)
            ;; (cellbrk)
            (when c-originating
              (setf (unst-setting-p c-originating) nil))))
        (trc nil "<<<< with-dataflow-management:" c-originating)))))

(defun dump-stale-path (used)
  (assert used)
  (when (typep used 'c-dependent)
    (let (any)
      (dolist (used-used (cd-useds used) any)
        (when (dump-stale-path used-used)
          (setf any t)
          (trc "stale-path" used :uses... used-used)))
      (when (or any (cd-stale-p used))
        (trc "stale" used)
        t))))

(defun dump-dependency-path (used user)
  (assert (and used user))
  (if (eql used user)
      (progn
        (trc "bingo---------------")
        (trc "user" user :uses...)
        t)
    (let (any)
      (dolist (used-user (cd-users used) any)
        (when (dump-dependency-path used-user user)
          (setf any t)
          (trc "user" used-user :uses... used))))))

(defun c-pending-set (c newvalue debug-tag)
  (declare (ignorable debug-tag))
  (assert (find c (cells (c-model c)) :key #'cdr))
  (if newvalue
        (if (assoc c *sw-pending*)
            (progn
              #+dfdbg (trc c "double-pending!!!>" c newvalue (assoc c *sw-pending*)))
          (let ((newpending (cons c newvalue)))
            (progn
              (assert (typep c 'c-dependent))
              (assert (not (eq :eternal-rest (md-state (c-model c)))))
              ;;(trc c "pending on, genealogy holdup: held, holder:" debug-tag c newvalue)
              ;;(dump-pending)
	      )
            (pushnew newpending *sw-pending* :test #'equal)))
    (let (#+dfdbg (p (assoc c *sw-pending*)))
      #+dfdbg (trc nil "clear from sw-pending" debug-tag c (remove-if (lambda (p)
                                                                        (not (eql c (car p))))
                                                             *sw-pending*))
      (setf *sw-pending* (delete (assoc c *sw-pending*) *sw-pending*))
      #+dfdbg (progn
                (trc c "pending off, genealogy holdup: held, holder:" debug-tag p
                  (count c *sw-pending* :key #'car))
                (dump-pending))
      ))
  newvalue)

(defmethod sw-pending ((c cell))
  (assert (find c (cells (c-model c)) :key #'cdr))
  (assoc c *sw-pending*))

(defmethod sw-pending ((s synapse))
  (sw-pending (syn-used s)))


