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


(defun c-link-ex (used &aux (user (car *c-calculators*)))
  (cond
   ((null used)
    ;
    ; no cell on used value so it is constant, but if a synapse is involved the constant
    ; must still be filtered thru that, albeit only this once
    ;
    (when *synapse-factory*
      (assert (car *c-calculators*)) ;; sanity-check
      (funcall *synapse-factory* nil (car *c-calculators*))))
    

   ((or (not (typep used 'c-user-notifying))
      (and (typep used 'c-dependent)
        (c-optimized-away-p used)))
    (return-from c-link-ex nil))

   (t 
    ;
    ; --------- debug stuff --------------
    (assert user)
    (trc nil "c-link > user, used" used)
    (count-it :c-link-entry)
    (when *c-debug*
      (assert (or (null *synapse-factory*)
                (functionp *synapse-factory*))
        ()
        "~s is not a function, but was supplied as a synapse factory between ~s and ~s. probably parentheses wrong, as in (- (^lr x 96))"
        *synapse-factory* used user))
     
    (let ((used-link
           (or 
            ;;  check if linked already
            ;;  /// looks like a bug: cannot have two synaptic dependencies on same
            ;;  /// cell slot...probably need to "name" the synapses just for this purpose
            ;;
            (c-find-used-link user used)
            ;;
            ;;  following may have been a goof, but i like it: let synapse factory
            ;;  decide not to produce a synapse, in which case dumb direct c-cell link
            ;;  gets created.
            ;;
            (bwhen (syn (and *synapse-factory*
                          (funcall *synapse-factory* used user)))
              (c-add-used user syn)
              (c-add-user used syn)
              ;;(trc used "c-link>     users now:" (mapcar #'celltrueuser (un-users used)))
              (trc nil "setting used to syn" syn used)
              syn)
            ;;
            ;;  make dumb link: used just tells user to rethink.
            ;;
            (progn
              (trc nil "c-link > new user,used " user used)
              (c-add-user used user)
              (c-add-used user used)
              used))))
      
      (assert used-link)
      (assert (position used-link (cd-useds user))
        ()
        "used-link ~a does not appear in useds ~a of user ~a"
        used-link (cd-useds user) user)
      
      (let ((mapn (- *cd-usagect*
                    (- (length (cd-useds user))
                      (or (position used-link (cd-useds user)) 0)))))      
        ;; (trc user "c-link> setting usage bit" user mapn used-link)
        (if (minusp mapn)
            (break "whoa. more than ~d used? i see ~d" *cd-usagect* (length (cd-useds user)))
          (cd-usage-set user mapn)))
      used-link))))
   
(defun cd-usage-set (c mapn)
  (when (typep c 'synapse)
    (setf (syn-relevant c) t))
  (setf (sbit (cd-usage c) mapn) 1))

(defun cd-usage-clear-all (c)
  (bit-and (cd-usage c)
           #*00000000000000000000000000000000
           t))

(defun c-find-used-link (user-cell used)
  "find any existing link to user-cell, the cell itself if direct or a synapse leading to it"
  (some (lambda (user)
          (if (typep user 'synapse)
              (when (eql user-cell (syn-user user))
                user) ;; the synapse is the used link
            (when (eql user-cell user)
              used))) ;; the link to used is direct (non-synaptic)
        (un-users used)))

(defun c-add-user (used user)
  (count-it :c-adduser)

  (typecase used
    (c-user-notifying (trc nil "c-add-user conventional >  user, used" used)                              
                     (pushnew user (un-users used)))

    (synapse (setf (syn-user used) user)))

  used)

(defun c-add-used (user used)
  (count-it :c-used)
  #+ucount (unless (member used (cd-useds user))
             (incf *cd-useds*)
             (when (zerop (mod *cd-useds* 100))
               (trc "useds count = " *cd-useds*)))
  (pushnew used (cd-useds user))
  ;;(trc user "c-add-used>  user , used" used (length (cd-useds user)))
  (cd-useds user))

(defun c-quiesce (c)
  (typecase c
    (cell 
     (trc nil "quiescing" c)
     (c-unlink-from-used c)
      (c-pending-set c nil :c-quiesce)
      ;;;   (setf (c-waking-state c) nil)
      ;;;   (when (eql :rpthead (c-model c))
      (trc nil "cell quiesce nulled cell awake" c))))

;-------------------------
                     
(defmethod c-unlink-from-used ((user c-dependent))
  (dolist (used (cd-useds user))
    (c-unlink-user used user))
  (setf (cd-useds user) nil))

(defmethod c-unlink-from-used (other)
  (declare (ignore other)))

(defun c-unlink-user (used  user)
  (etypecase used
    (c-user-notifying 
     ;;(trc user "user unlinking from used" user used)
     (setf (un-users used) (delete user (un-users used))))

    (synapse
     (let ((syn used))
       (setf (syn-user syn) nil)
       (c-unlink-user (syn-used syn) syn)))))
