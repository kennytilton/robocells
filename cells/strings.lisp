;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
;;;
;;; Copyright ? 1995,2003 by Kenneth William Tilton.
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

(defmacro case$ (stringForm &rest cases)
  (let ((v$ (gensym))
        (default (or (find 'otherwise cases :key #'car)
                     (find 'otherwise cases :key #'car))))
     (when default
       (setf cases (delete default cases)))
     `(let ((,v$ ,stringForm))
         (cond
          ,@(mapcar (lambda (caseForms)
                        `((string-equal ,v$ ,(car caseForms)) ,@(rest caseForms)))
                    cases)
          (t ,@(or (cdr default) `(nil)))))))

;--------

(defmethod shortc (other)
  (declare (ignorable other))
  (concatenate 'string "noshortc" (symbol-name (class-name (class-of other)))))

(defmethod longc (other) (shortc other))

(defmethod shortc ((nada null)) nil)
(defmethod shortc ((many list))
   (if (consp (cdr many))
       (mapcar #'shortc many)
     (conc$ (shortc (car many)) " " (shortc (cdr many)))))
(defmethod shortc ((self string)) self)
(defmethod shortc ((self symbol)) (string self))
(defmethod shortc ((self number)) (num$ self))
(defmethod shortc ((self character)) (string self))

;-----------------------

(defun strloc$ (substr str)
   (when (and substr str (not (string= substr "")))
     (search substr str)))

(defun make$ (&optional (size 0) (char #\space))
   (make-string size :initial-element (etypecase char
                                        (character char)
                                        (number (code-char char)))))

(DEFUN space$ (size)
  (make$ size))

(defun char$ (char)
   (make$ 1 char))

(defun conclist$ (ss)
   (when ss
     (reduce (lambda (s1 s2) (concatenate 'string s1 s2)) ss)))

(defun conc$ (&rest ss)
  (let (result)
    (dolist (s ss result)
      (when s
        (setf result (if result
                         (concatenate 'string result (shortc s))
                       (shortc s)))))))

(defun left$ (s n)
   (subseq s 0 (max (min n (length s)) 0)))

(defun mid$ (s offset length)
  (let* ((slen (length s))
         (start (min slen (max offset 0)))
         (end (max start (min (+ offset length) slen))))
   (subseq s start end)))

(defun seg$ (s offset end)
  (let* ((slen (length s))
         (start (min slen (max offset 0)))
         (end (max start (min end slen))))
   (subseq s start end)))

(defun right$ (s n)
   (subseq s (min n (length s))))

(defun insert$ (s c &optional (offset (length s)))
     (conc$ (subseq s 0 offset)
       (string c)
       (subseq s offset)))

(defun remove$ (s offset)
     (conc$ (subseq s 0 (1- offset))
       (subseq s offset)))

(defun trim$ (s)
   (assert (or (null s) (stringp s)))
   (string-trim '(#\space) s))

(defun trunc$ (s char)
   (let ((pos (position char s)))
      (if pos
         (subseq s 0 pos)
         s)))

(defun abbrev$ (long$ max)
  (if (<= (length long$) max)
        long$
      (conc$ (left$ long$ (- max 3)) "...")))

(defmethod empty ((nada null)) t)
(defmethod empty ((c cons))
  (and (empty (car c))
       (empty (cdr c))))
(defmethod empty ((s string)) (empty$ s))
(defmethod empty (other) (declare (ignorable other)) nil)

(defun empty$ (s)
   (or (null s)
       (if (stringp s)
          (string-equal "" (trim$ s))
          #+not (trc nil "empty$> sees non-string" (type-of s)))
       ))

(defmacro find$ (it where &rest args)
  `(find ,it ,where ,@args :test #'string-equal))

(defmethod num$ ((n number))
   (format nil "~d" n))

(defmethod num$ (n)
   (format nil "~d" n))

(defun normalize$ (s)
   (etypecase s
     (null "")
     (string (string-downcase s))
     (symbol (string-downcase (symbol-name s)))))

(defun down$ (s)
   (string-downcase s))

(defun lower$ (s)
   (string-downcase s))

(defun up$ (s)
   (string-upcase s))

(defun upper$ (s)
   (string-upcase s))

(defun equal$ (s1 s2)
   (if (empty$ s1)
      (empty$ s2)
      (when s2
         (string-equal s1 s2))))

(defun min$ (&rest ss)
   (cond
    ((null ss) nil)
    ((null (cdr ss)) (car ss))
    (t (let ((rmin$ (apply #'min$ (cdr ss))))
          (if (string< (car ss) rmin$)
             (car ss) rmin$)))))

(defun numeric$ (s &optional trimmed)
   (every (lambda (c) (digit-char-p c)) (if trimmed (Trim$ s) s)))

(defun alpha$ (s)
   (every (lambda (c) (alpha-char-p c)) s))

(defmacro assoc$ (item alist &rest kws)
   `(assoc ,item ,alist :test #'equal ,@kws))

(defmacro member$ (item list &rest kws)
   `(member ,item ,list :test #'string= ,@kws))

(defun match-left$ (a b) 
  (string-equal a (subseq b 0 (length a))))

(defparameter *return$* (conc$ (char$ #\return) (char$ #\linefeed)))
(defparameter *LF$* (string #\linefeed))
