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

(defparameter *fmdbg* nil)

(eval-when (compile eval load)
  (export '(make-part mkpart fm-other fm-traverse fm-descendant-typed do-like-fm-parts
             container-typed *fmdbg*)))

(defun make-part (partname partclass &rest initargs)
  ;;(trc "make-part > name class" partname partclass)
  (when partclass ;;a little programmer friendliness
    (apply #'make-instance partclass :md-name partname initargs)))

(defmacro mkpart (md-name (mdclass) &rest initargs)
  `(make-part ',md-name ',mdclass ,@initargs))

(defmethod make-partspec ((partclass symbol))
  (make-part partclass partclass))

(defmethod make-partspec ((part model))
  part)

(defmacro upper (self &optional (type t))
  `(container-typed ,self ',type))

(defmethod container (self) (fmparent self))

(defmethod container-typed ((self model-object) type)
   (assert self)
   (let ((parent (container self))) ;; fm- or ps-parent
      (cond
       ((null parent) nil)
       ((typep parent type) parent)
       (t (container-typed parent type)))))

(defun fm-descendant-typed (self type)
  (when self
    (or (find-if (lambda (k) (typep k type)) (kids self))
        (some (lambda (k)
                  (fm-descendant-typed k type)) (kids self)))))

(defun fm-descendant-named (parent name &key (must-find t))
   (fm-find-one parent name :must-find must-find :global-search nil))

(defun fm-ascendant-named (parent name)
   (when parent
      (or (when (eql (md-name parent) name)
             parent)
          (fm-ascendant-named (fmparent parent) name))))

(defun fm-ascendant-typed (parent name)
   (when parent
      (or (when (typep parent name)
             parent)
          (fm-ascendant-typed (fmparent parent) name))))

(defun fm-ascendant-some (parent somefunction)
   (when (and parent somefunction)
     (or (funcall somefunction parent)
         (fm-ascendant-some (fmparent parent) somefunction))))

(defun fm-ascendant-if (self iffunction)
   (when (and self iffunction)
     (or (when (funcall iffunction self)
           self)
         (fm-ascendant-if .parent. iffunction))))

(defun fm-ascendant-common (d1 d2)
  (fm-ascendant-some d1 (lambda (node)
                            (when (fm-includes node d2)
                              node))))

(defun fm-collect-if (tree test)
  (let (collection)
    (fm-traverse tree (lambda (node)
                        (when (funcall test node)
                          (push node collection))))
    (nreverse collection)))

(defun fm-max (tree key)
  (let (max)
    (fm-traverse tree (lambda (node)
                        (if max
                            (setf max (max max (funcall key node)))
                          (setf max (funcall key node))))
      :global-search nil)
    max))


(defun fm-traverse (family applied-fn &key skipnode skiptree (global-search t) (opaque nil))
  (progn ;; wtrc (0 1600 "fm-traverse2" family)
    (labels ((tv-family (fm skippee)
               ;;(when *fmdbg* (trc "tv-family" fm))
               (when (and (typep fm 'model-object)
                       (not (eql fm skippee)))
                 (let ((outcome (and (not (eql skipnode fm))
                                  (funcall applied-fn fm))))
                   (unless (and outcome opaque)
                     (dolist (kid (sub-nodes fm))
                       (tv-family kid nil)))))))

      (do ((fm family (when global-search
                        (fmparent fm)))
           (skip skiptree fm))
          ((not fm) nil)
        (tv-family fm skip)))))

#+old
(defun fm-traverse (family applied-fn &key skipnode skiptree (global-search t) (opaque nil))
  ;;
  ;;(when *fmdbg* (trc "fm-traverse" family skiptree skipnode global-search))
  ;;
  (wtrc (0 1600 "fm-traverse2" family)
    (when family
      (labels (
               (tv-family (fm)
                 ;;(when *fmdbg* (trc "tv-family" fm))
                 (when (and (typep fm 'model-object)
                         (not (eql fm skiptree)))
                   (let ((outcome (and (not (eql skipnode fm)) ;; skipnode new 990310 kt
                                    (funcall applied-fn fm))))
                     (unless (and outcome opaque)
                       (dolist (kid (sub-nodes fm))
                         (tv-family kid))
                       ;(tv-family (mdvalue fm))
                       )))))
        (tv-family family)
        (when global-search
          (fm-traverse (fm-ps-parent family) applied-fn :skiptree family :skipnode skipnode)
          ))
      nil)))

(defmethod sub-nodes (other)
  (declare (ignore other)))

(defmethod sub-nodes ((self family))
  (kids self))

(defmethod fm-ps-parent ((self model-object))
  (fmparent self))

(defmacro with-like-fm-parts ((partsvar (self likeclass)) &body body)
   `(let (,partsvar)
       (fm-traverse ,self (lambda (node)
                              ;;(trc "with like sees node" node (type-of node) ',likeclass)
                              (when (typep node ',likeclass)
                                 (push node ,partsvar)))
         :skipnode ,self
         :global-search nil
         :opaque t)
       (setf ,partsvar (nreverse ,partsvar))
       (progn ,@body)))

(defmacro do-like-fm-parts ((partvar (self likeclass) &optional returnvar) &body body)
   `(progn
     (fm-traverse ,self (lambda (,partvar)
                            (when (typep ,partvar ',likeclass)
                               ,@body))
       :skipnode ,self
       :global-search nil
       :opaque t)
       ,returnvar)
   )

;;
;; family member finding
;;

#|
 (defun fm-member-named (kidname kids)
  (member kidname kids :key #'md-name))
 |#

(defun true-that (that) (declare (ignore that)) t)
;;
;; eventually fm-find-all needs a better name (as does fm-collect) and they
;; should be modified to go through 'gather', which should be the real fm-find-all
;;
(defun fm-gather (family &key (test #'true-that))
     (packed-flat!
      (cons (when (funcall test family) family)
        (mapcar (lambda (fm)
                    (fm-gather fm :test test))
          (kids family)))))

(defun fm-find-all (family md-name &key (must-find t) (global-search t))
     (let ((matches (catch 'fm-find-all
                             (with-dynamic-fn
                              (traveller (family)
                               (with-dynamic-fn
                                (filter (kid) (eql md-name (md-name kid)))
                                (let ((matches (remove-if-not filter (kids family))))
                                   (when matches
                                        (throw 'fm-find-all matches)))))
                              (fm-traverse family traveller :global-search global-search)))))
        (when (and must-find (null matches))
           (setf *stop* t)
          (break "fm-find-all > *stop*ping...did not find ~a ~a ~a" family md-name global-search)
          ;; (error 'fm-not-found (list md-name family global-search))
          )
        matches))

(defun fm-find-next (fm test-fn)
  (fm-find-next-within fm test-fn))

(defun fm-find-next-within (fm test-fn &optional upperbound &aux (fmparent (unless (eql upperbound fm)
                                                                              (fmparent fm))))
   (let ((sibs (and fmparent (rest (member fm (kids fmparent))))))
      (or (dolist (s sibs)
             (let ((winner (fm-find-if s test-fn)))
                (when winner (return winner))))
          (if fmparent
             (fm-find-next-within fmparent test-fn upperbound)
             (fm-find-if fm test-fn)))))

(defun fm-find-prior (fm test-fn)
  (fm-find-prior-within fm test-fn))

(defun fm-find-prior-within (fm test-fn &optional upperbound &aux (fmparent (unless (eql upperbound fm)
                                                                              (fmparent fm))))
     (let ((sibs (and fmparent (kids fmparent))))
        (or (and sibs (do ((s sibs (rest s))
                           (last-ok nil  (or next-ok last-ok))
                           (next-ok nil))
                          ((or (null s)
                               (eql fm (first s))) last-ok)
                        (setf next-ok (fm-find-last-if (car s) test-fn))))
            (if fmparent
                (fm-find-prior-within fmparent test-fn upperbound)
              (fm-find-last-if fm test-fn)))))

(defun fm-find-last-if (family test-fn)
     (let ((last))
        (or (and (kids family)
                     (dolist (k (kids family) last)
                          (setf last (or (fm-find-last-if k test-fn) last))))
             (when (funcall test-fn family)
                  family))))

(defun fm-prior-sib (fm &optional (test-fn #'true-that) &aux (fmparent (fmparent fm)))
  (assert fmparent)
  (let ((sibs (and fmparent (kids fmparent))))
     (and sibs (do ((s       sibs (rest s))
                    (last-ok nil  (or next-ok last-ok))
                    (next-ok nil))
                   ((or (null s)
                        (eql fm (first s))) last-ok)
                 (setf next-ok (when (funcall test-fn (car s))
                                 (car s)))))))

(defun fm-next-sib-if (self test-fn)
     (some test-fn (cdr (member self (kids (fmparent self))))))

(defun fm-next-sib (self)
     (car (cdr (member self (kids (fmparent self))))))

(defmacro ^fm-next-sib (&optional (self 'self))
     (let ((s (gensym)))
        `(let ((,s ,self))
             (car (cdr (member ,s (^kids (fmparent ,s))))))))

(defun find-prior (self sibs &key (test #'true-that))
  (assert (member self sibs)) ;; got this by accidentally having toolbar kids dependent..on second calc,
  ;;                             all newkids got over, and when old kids tried to recalculate...not in sibs!!
  (unless (eql self (car sibs))
    (labels
        ((fpsib (rsibs &aux (psib (car rsibs)))
                (assert rsibs () "~&find-prior > fpsib > self ~s not found to prior off" self)
                (if (eql self (cadr rsibs))
                   (when (funcall test psib) psib)
                   (or (fpsib (cdr rsibs))
                       (when (funcall test psib) psib)))))
      (fpsib sibs))))

(defun fm-find-if (family test-fn &key skiptopp) ;; 99-03 kt why is thsi depth-first?
  (assert test-fn)
  (when family
    (or (dolist (b (sub-nodes family))
          (let ((match (fm-find-if b test-fn)))
             (when match (return match))))
        (when (and (not skiptopp)
                   (funcall test-fn family))
          family))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  family ordering
;;;;
(defun fm-kid-add (fmparent kid &optional before)
     (assert (or (null (fmparent kid)) (eql fmparent (fmparent kid))))
   (assert (typep fmparent 'family))
     (setf (fmparent kid) fmparent)
     (fm-kid-insert kid before))

(defun fm-kid-insert-last (goal &aux (fmparent (fmparent goal)))
     (setf (kids fmparent) (nconc (kids fmparent) (list goal))))

(defun fm-kid-insert-first (goal &aux (fmparent (fmparent goal)))
     (setf (kids fmparent) (cons goal (kids fmparent))))

(defun fm-kid-insert (kid &optional before &aux (dakids (kids (fmparent kid))))
  (assert (or (null before) (eql (fmparent kid) (fmparent before))))
  (setf (kids (fmparent kid))
          (if before
             (if (eql before (car dakids))
                (cons kid dakids)
                (let ((cell (member before dakids)))
                   (rplaca cell kid)
                   (rplacd cell (cons before (cdr cell)))
                   (cons (car dakids) (rest dakids))))
             (if dakids
                (progn
                  (rplacd (last dakids) (cons kid nil))
                  (cons (car dakids) (rest dakids)))
                (cons kid dakids)))))

(defun fm-kid-remove (kid &key (quiesce t) &aux (parent (fmparent kid)))
  (when quiesce
    (fm-quiesce-all kid))
  (when parent
    (setf (kids parent) (remove kid (kids parent)))
    ;; (setf (fmparent kid) nil) gratuitous housekeeping caused ensuing focus echo
    ;; image-invalidate to fail since no access to containing window via fmparent chain
    ))

(defun fm-quiesce-all (md)
  (md-quiesce md)
  (dolist (kid (kids md))
    (when (and kid (not (md-untouchable kid)))
      (fm-quiesce-all kid)))
  md)


(defun fm-kid-replace (oldkid newkid &aux (fmparent (fmparent oldkid)))
     (assert (member oldkid (kids fmparent)) ()
        "~&oldkid ~s not amongst kids of its fmparent ~s"
        oldkid fmparent)
     (when fmparent ;; silly test given above assert--which is right?
        (assert (typep fmparent 'family))
          (setf (fmparent newkid) fmparent)
          (setf (kids fmparent) (substitute newkid oldkid (kids fmparent)))
          ;;(rplaca (member oldkid (kids fmparent)) newkid)
          newkid))

;----------------------------------------------------------
;;
;; h i g h  -  o r d e r   f a m i l y   o p s
;;
;; currently not in use...someday?
(defmacro ^fm-min-max-kid (min-max slot-name &key (default 0) test (fmparent 'self))
   (let ((best (copy-symbol 'best))
         (kid (copy-symbol 'kid))
         )
      `(let ((,best ,default))
          (dolist (,kid (^kids ,fmparent) ,best)
            ,(if test
                `(when (funcall ,test ,kid)
                   (setf ,best (funcall ,min-max ,best (,slot-name ,kid))))
                `(bif (slotvalue (,slot-name ,kid))
                    (setf ,best (funcall ,min-max ,best slotvalue))
                    (break "nil slotvalue ~a in kid ~a of parent ~a"
                           ',slot-name ,kid ,fmparent)))))))

(defmacro ^fm-min-kid (slot-name &key (default 0) test (fmparent 'self))
     `(^fm-min-max-kid #'min-if ,slot-name
       :default ,default
       :test ,test
       :fmparent ,fmparent))

(defmacro ^fm-max-kid (slot-name &key (default 0) test (fmparent 'self))
     `(^fm-min-max-kid #'max-if ,slot-name
       :default ,default
       :test ,test
       :fmparent ,fmparent))

(defmacro ^fm-max-sib (slot-name &key (default 0) test)
     `(^fm-max-kid ,slot-name :default ,default
       :test ,test
       :fmparent (fmparent self)))

(defmacro ^fm-max-sib-other (slot-name &key (default 0))
     `(with-dynamic-fn (tester (sib) (not (eql self sib)))
       (^fm-max-kid ,slot-name :default ,default
        :test tester
        :fmparent (fmparent self))))

(defmacro ^sib-named (name)
   `(find ,name (^kids (fmparent self)) :key #'md-name))


(defmacro fm-other (md-name &key (starting 'self) skiptree (test '#'true-that))
  `(fm-find-one ,starting ,(if (consp md-name)
                               `(list ',(car md-name) ,(cadr md-name))
                             `',md-name)
                :must-find t
                :skiptree ,skiptree
                :global-search t
                :test ,test))

(defmacro fm-otherx (md-name &key (starting 'self) skiptree)
   (if (eql starting 'self)
      `(or (fm-find-one ,starting ,(if (consp md-name)
                                      `(list ',(car md-name) ,(cadr md-name))
                                      `',md-name)
             :must-find t
             :skiptree ,skiptree
             :global-search t))
      `(fm-find-one ,starting ,(if (consp md-name)
                                  `(list ',(car md-name) ,(cadr md-name))
                                  `',md-name)
         :must-find t
         :skiptree ,skiptree
         :global-search t)))

(defun fm-other-v (md-name starting &optional (global-search t))
    (fm-find-one starting md-name
          :must-find nil
          :global-search global-search))

(defmacro fm-otherv? (md-name &optional (starting 'self) (global-search t))
  `(fm-other-v ,md-name ,starting ,global-search))

(defmacro fm-other? (md-name &optional (starting 'self) (global-search t))
    `(fm-find-one ,starting ,(if (consp md-name)
                                               `(list ',(car md-name) ,(cadr md-name))
                                               `',md-name)
          :must-find nil
          :global-search ,global-search))

(defmacro fm-other! (md-name &optional (starting 'self))
    `(fm-find-one ,starting ,(if (consp md-name)
                                                 `(list ',(car md-name) ,(cadr md-name))
                                                `',md-name)
        :must-find t
        :global-search nil))

(defmacro fm-other?! (md-name &optional (starting 'self))
   `(fm-find-one ,starting ,(if (consp md-name)
                                         `(list ',(car md-name) ,(cadr md-name))
                                  `',md-name)
     :must-find nil
     :global-search nil))

(defmacro fm-collect (md-name &key (must-find t))
   `(fm-find-all self ',md-name :must-find ,must-find)) ;deliberate capture

(defmacro fm-map (fn md-name)
         `(mapcar ,fn (fm-find-all self ',md-name))) ;deliberate capture

(defmacro fm-mapc (fn md-name)
   `(mapc ,fn (fm-find-all self ',md-name))) ;deliberate capture

(defun fm-pos (goal &aux (fmparent (fmparent goal)))
   (when fmparent
           (or (position goal (kids fmparent))
                               (length (kids fmparent))))) ;; ?!!

(defmacro fm-count-named (family md-name &key (global-search t))
    `(length (fm-find-all ,family ,md-name
                 :must-find nil
                 :global-search ,global-search)))
;---------------------------------------------------------------

(defun fm-top (fm &optional (test #'true-that) &aux (fmparent (fmparent fm)))
    (cond ((null fmparent) fm)
                ((not (funcall test fmparent)) fm)
                (t (fm-top fmparent test))))

(defun fm-first-above (fm &key (test #'true-that) &aux (fmparent (fmparent fm)))
    (cond ((null fmparent) nil)
              ((funcall test fmparent) fmparent)
              (t (fm-first-above fmparent :test test))))

(defun fm-nearest-if (test fm)
  (when fm
    (if (funcall test fm)
       fm
       (fm-nearest-if test (fmparent fm)))))

(defun fm-includes (fm sought)
  (fm-ancestorp fm sought))

(defun fm-ancestorp (fm sought)
   (assert fm)
   (when sought
      (or (eql fm sought)
          (fm-includes fm (fmparent sought)))))

(defun fm-kid-containing (fmparent descendant)
   (with-dynamic-fn (finder (node) (not (eql fmparent node)))
     (fm-top descendant finder)))

(defun make-name (root &optional subscript)
   (if subscript (list root subscript) root))

(defun name-root (md-name)
   (if (atom md-name) md-name (car md-name)))

(defun name-subscript (md-name)
   (when (consp md-name) (cadr md-name)))

(defun fm-find-one (family md-name &key (must-find t)
                           (global-search t) skiptree (test #'true-that))
  (flet ((matcher (fm)
                  (trc nil "fm-find-one matcher sees" md-name fm (md-name fm))
                  (when (and (eql (name-root md-name)
                                  (or (md-name fm) (c-class-name (class-of fm))))
                             (or (null (name-subscript md-name))
                                 (eql (name-subscript md-name) (fm-pos fm)))
                             (funcall test fm))
                    (throw 'fm-find-one fm))))
    #-lispworks (declare (dynamic-extent matcher))
    (trc nil "fm-find-one> entry " md-name family)    
    (let ((match (catch 'fm-find-one
                        (fm-traverse family #'matcher
                                     :skiptree skiptree
                                     :global-search global-search))))
      (when (and must-find (null match))
        (trc nil "fm-find-one > erroring fm-not-found" family md-name must-find global-search)
        ;;(inspect family)
        (let ((*fmdbg* family))
          (fm-find-one family md-name :must-find nil :global-search global-search)
          (setf *stop* t)
          ;;(trc "fm-find-one > *stop*ping...did not find" family md-name global-search)
          (break "fm-find-one > *stop*ping...did not find ~a ~a ~a" family md-name global-search)
          
          ))
      match)))

(defun fm-find-kid (self name)
   (find name (kids self) :key #'md-name))

(defun fm-kid-typed (self type)
   (assert self)
  (find type (kids self) :key #'type-of))

(defun kidno (self)
  (unless (typep self 'model-object)
    (break "not a model object ~a" self))
  (when (and self (fmparent self))
    (assert (member self (kids (fmparent self))))
    (position self (kids (fmparent self)))))


