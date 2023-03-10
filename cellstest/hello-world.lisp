;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
;;;
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

(defmodel computer ()
  ((happen :cell :ephemeral :initform (cv nil) :accessor happen)
   (location :cell t
             :initform (c? (case (^happen)
                              (:leave :away)
                              (:arrive :at-home)
                              (t (c-value c))))
             :accessor location)
   (response :cell :ephemeral :initform nil :initarg :response :accessor response)))

(def-c-echo response(self newResponse oldResponse)
  (when newResponse
    (format t "~&Computer: ~a" newResponse)))

(def-c-echo happen()
  (when new-value
    (format t "~&Happen: ~a" new-Value)))

(defun hello-world ()
  (let ((dell (to-be
               (make-instance 'computer
                 :response (c? (bWhen (h (happen self))
                                 (if (eql (^location) :at-home)
                                     (case h
                                       (:knock-knock "Who's there?")
                                       (:world "Hello, world."))
                                   "<silence>")))))))
    (dotimes (n 2)
      (setf (happen dell) :knock-knock))

    (setf (happen dell) :arrive)
    (setf (happen dell) :knock-knock)
    (setf (happen dell) :world)
    (values)))

#+test
(hello-world)

#+test
(trace sm-echo)


#| Output

Happen: KNOCK-KNOCK
Computer: <silence>
Happen: KNOCK-KNOCK
Computer: <silence>
Happen: ARRIVE
Happen: KNOCK-KNOCK
Computer: Who's there?
Happen: WORLD
Computer: Hello, world.

|#

