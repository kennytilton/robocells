;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cello; -*-
;;______________________________________________________________
;;
;;
;;
;;     Cell Basics 
;;
;;   Copyright © 1996,2003 by Kenny Tilton. All rights reserved.
;;

(in-package :cells)

#|

Here is a minimal primer on Cells, just enough for you to
keep up with the next tutorial. That will be a substantial project
in which we develop a CLOS object inspector.

The Inspector project will give you a feel for what it is like to 
program with Cells and Cello /after/ you are fluent in the
technology. The intent is not to teach you Cello, rather to
motivate your learning it.

So why the primer on Cells? If things like C? and CV and DEF-C-ECHO 
do not mean anything to you, the Hunh? Factor will be overwhelming.


Cells
-----
Think of a CLOS slot as a cell in a paper spreadsheet, a financial
modeling tool popular enough to make VisiCalc the first business
killer app for microcomputers.

As a child I watched my father toil at home for hours over paper 
spreadsheets with pencil and slide rule. After he changed one value, 
he had to propagate that change to other cells by first remembering 
which other ones included the changed cell in their computation. 
Then he had to do the calculations for those, erase, enter...
and then repeating that process to propagate those changes in a 
cascade across the paper.

VisiCalc let my father take the formula he had in mind and 
put it in (declare it to) the electronic spreadsheet. Then VisiCalc 
could do the tedious work: recalculating, knowing what to recalculate, 
and knowing in what order to recalculate.

Cells do for programmers what electronic spreadsheets did for my father.
Without Cells, CLOS slots are like cells of a paper spreadsheet. 
A single key-down event can cause a cascade of change throughout an 
application. The programmer has to arrange for it all to happen,
all in the right order: delete any selected text, insert 
the new character, re-wrap the text, update the undo mechanism, revisit
the menu statuses ("Cut" is no longer enabled), update the scroll bars,
possibly scroll the window, flag the file as unsaved...

With Cells, the programmer looks at program state differently. One
asks, "How could I compute, at any point of runtime, a value for 
a given slot of an arbitrary instance, based only on other runtime state 
(other slots of other instances)." Great fun, by the way, as well as
enforcing good programming practices like encapsulation.

An example will help. Consider indeed the state of the "Cut" menu item. 
In some applications, programmers have a dozen places in their code
where they tend to the status of the Cut menu item. One might be:

(defun do-clear (edit-structure)
  (when (selected-range edit-structure)
    <set up undo>
    <toss selected text>
    <etc><etc>
    (menu-item-enable *edit-cut* nil)
    (menu-item-enable *edit-copy* nil)
    (menu-item-enable *edit-clear* nil)))

Other programmers wait until the user clicks on the Edit menu, 
then decide just-in-time from program state whether the Cut item 
should be enabled:

(defmethod prep-for-display ((m edit-menu))
  <lotsa other stuff>
  (when (typep (focus *app*) 'text-edit-widget)
    (menu-item-enable (find :cut (items m) :key #'item-name)
      (not (null (selected-range (focus *app*)))))))

This latter programmer is ready for Cells, because they
have already shifted from imperative to declarative thinking;
they have learned to write code that works based not on what 
has happened lately, but instead only on the current program 
state (however it got that way). 

The Cell programmer writes:

(make-instance 'menu-item
  :name :cut
  :label "Cut"
  :cmd-key +control-x+
  :actor #'do-cut
  :enabled (c? (when (typep (focus *app*) 'text-edit-widget)
                 (not (null (selected-range (focus *app*)))))))

...and now they can forget the menu item exists as they work
on the rest of the application. The menu-item enabled status
will stay current (correct) as the selected-range changes
and as the focus itself changes as the user moves from field
to field.

That covers the spirit of Cells. Now let's look at the syntax
and mechanics, with examples you can execute once you have 
loaded the Cells package. See the read-me.txt file in the
root directory into which the Cello software was unzipped.

We'll model a falling stone, where the distance fallen is half
the product of the acceleration (due to gravity) and the
square of the time falling.

|#

(in-package :cells)

(defmodel stone ()
  ((accel :cell t :initarg :accel :initform 0 :accessor accel)
   (time-elapsed :cell t :initarg :time-elapsed
     :initform (cv 0)
     :accessor time-elapsed)
   (distance :cell t :initarg :distance :initform 0 :accessor distance))
  (:default-initargs
      :distance (c? (/ (* (accel self)
                         (expt (time-elapsed self) 2))
                      2))))

(def-c-echo accel ((self stone) new old old-bound-p)
  (trc "ECHO accel" :new new :old old :oldp old-bound-p)) ;; TRC provides print diagnostics

(def-c-echo time-elapsed ((self stone)) ;; short form (I'm lazy)
  (trc "ECHO time-elapsed" :new new-value :old old-value :oldp old-value-boundp))

(def-c-echo distance ((self stone))
  (format t "~&ECHO distance fallen: ~d feet" new-value))


#|
Let's look at non-standard syntax found in the forms above,
in the order in which they appear:

    (defmodel ...

defmodel is just a defclass wrapper which also sets up plumbing for Cells.

   ... :cell t ...

Without this option, a model instance slot cannot be powered
by a cell (and cell slot access overhead is avoided). 

With this option, one can specify what kind of Cell
is to be defined: ephemeral, delta or t (normal). We'll leave 
those esoteric cell slot types for another tutorial and just 
specify t to get normal cells (the ones used 99% of the time). 

   time-elapsed ... :initform (cv 0)...

(CV <value>) allows the cellular slot (or "cell", for short) 
to be setf'ed. These are inputs to the dataflow,
which usually flows from C? to C? but has to start somewhere. 
Since modern interactve applications are event-driven, in
real-world Cello apps most CV dataflow inputs are slots closely
corresponding to some system value, such as the position slots
of a cell-powered Mouse class. Moving on...

A naked value such as the 32 supplied for accel cannot be changed; a 
runtime error results from any such attempt. This makes Cells faster,
because some plumbing can be skipped: no dependency gets recorded between
the distance traveled and the acceleration. On the other hand, a more
elaborate model might have the acceleration varying according to the distance
between the stone and Earth (in which case we get into an advance
topic for another day, namely how to handle circularity.)

Next: (:default-initargs
         :distance (c? (/ (* (accel self)
                             (expt (time-elapsed self) 2))
                          2)

C? associates a rule with a cellular slot (or "cell", for short). Any
read operation on another cell (directly or during a function call)
establishes a dependency of distance on that cell -- unless that cell
can never change. Why would a Cell not be able to change?

Cell internals enforce a rule that a Cell with a naked value (ie, not wrapped 
in CV or C?) cannot be changed by client code (ok, (setf slot-value) is a backdoor).
Cell internals enforce this, simply to make possible the optimization
of leaving off the overhead of recording a pointless dependency.

Next: (def-c-echo...

Here is the signature for the DEF-C-ECHO macro:

   (defmacro def-c-echo (slotname (&optional (selfarg 'self)
                                    (newvarg 'new-value)
                                    (oldvarg 'old-value)
                                    (oldvargboundp 'old-value-boundp))
                      &body echobody) ....)

def-c-echo defines a generic method one can specialize on any of the four
parameters. The method gets called when the slot value changes, and during 
initial processing by:

    (to-be....)

TO-BE brings a new model instance to life, including calling
any echos defined for cellular slots. 

Why not just do this in initialize-instance? We build complex 
models in the form of a tree of many model instances, any of 
which may depend on some other model instance to calculate 
some part of its state. Models find the one they are curious 
about by searching the tree.

This means we cannot just bring a model instance to life at
make-instance time; some cell rule may go looking for another
model instance. We must wait until the instance is 
embedded in the larger model tree, then we can kick off to-be.

Likewise, when we yank an instance from the larger model we
will call NOT-TO-BE on it.

The good news is that unless I am doing little tutorial examples
I never think about calling TO-BE. Trees are implemented in part
by a "kids" (short for "children") cell. The echo on that cell
calls TO-BE on new kids and NOT-TO-BE on kids no longer in the list.

Now evaluate the following:

|#

(defparameter *s2* (to-be (make-instance 'stone
                            :accel 32 ;; (constant) feet per second per second
                            :time-elapsed (cv 0))))

#|

...and observe:
0> ECHO accel :NEW 32 :OLD NIL :OLDP NIL
0> ECHO time-elapsed :NEW 0 :OLD NIL :OLDP NIL
ECHO distance fallen: 0 feet


Getting back to the output shown above, why echo output on a new instance?

When we call TO-BE we want the instance to come to life. That means 
evaluating every rule so the dependencies get established, and 
propagating cell values outside the model (by calling the echo
methods) to make sure the model and outside world (if only the
system display) are consistent.

;-----------------------------------------------------------
Now let's get moving:

|#

(setf (time-elapsed *s2*) 1)

#|
...and observe:
0> ECHO time-elapsed :NEW 1 :OLD 0 :OLDP T
ECHO distance fallen: 16 feet

behind the scenes:
- the slot value time-elapsed got changeed from 0 to 1
- the time-elapsed echo was called
- dependents on time-elapsed (here just  distance) were recalculated
- go to the first step, this time for thhe distance slot

;-----------------------------------------------------------
To see some optimizations at work, set the cell time-elapsed to
the same value it already has:
|# 

(setf (time-elapsed *s2*) 1) 

#| observe:
nothing, since the slot-value did not in fact change.

;-----------------------------------------------------------
To test the enforcement of the Cell stricture against
modifying cells holding naked values:
|#

(handler-case
    (setf (accel *s2*) 10)
  (t (error) (trc "error is" error)
    error))

#| Observe:
c-setting-debug > constant  ACCEL in STONE may not be altered..init to (cv nil)
0> error is #<SIMPLE-ERROR @ #x210925f2>

;-----------------------------------------------------------
Nor may ruled cells be modified arbitrarily:
|#

(handler-case
    (setf (distance *s2*) 42)
  (t (error) (trc "error is" error)
    error))

#| observe:
c-setting-debug > ruled  DISTANCE in STONE may not be setf'ed
0> error is #<SIMPLE-ERROR @ #x2123e392>

;-----------------------------------------------------------
Aside from C?, CV, and DEF-C-ECHO, another thing you will see
in Cello code is how complex views are constructed using
the Family class and its slot KIDS. Every model-object has a 
parent slot, which gets used along with a Family's kids slot to
form simple trees navigable up and down.

Model-objects also have slots for mdName and mdValue (don't
worry camelcase-haters, that is a declining feature of my code).
mdName lets the Family trees we build be treated as namespaces.
mdValue just turns out to be very handy for a lot of things. For
example, a check-box instance needs some place to indicate its 
boolean state. 

Now let's see Family in action, using code from the Handbook of
Silly Examples. All I want to get across is that a lot happens
when one changes the kids slot. It happens automatically, and
it happens transparently, following the dataflow implicit in the
rules we write, and the side-effects we specify via echo functions.

The Silly Example below just shows the Summer (that which sums) getting
a new mdValue as the kids change, along with some echo output. In real-world 
applications, where kids represent GUI elements often dependent on
each other, vastly more can transpire before a simple push into a kids
slot has run its course.

Evaluate:
|#

(defmodel Summer (Family)
  ()
  (:default-initargs
      :kids (cv nil) ;; or we cannot add any addend kids later
    :mdValue (c? (reduce #'+ (kids self)
                   :initial-value 0
                   :key #'mdValue))))

(def-c-echo .mdValue ((self Summer))
  (trc "The sum of the values of the kids is" new-value))

(def-c-echo .kids ((self Summer))
  (trc "The values of the kids are" (mapcar #'mdValue new-value)))

;-----------------------------------------------------------
; now just evaluate each of the following forms one by one,
; checking results after each to see what is going on
;
(defparameter *f1* (to-be (make-instance 'Summer)))

#|
observe:
0> The sum of the values of the kids is 0
0> The values of the kids are NIL

;----------------------------------------------------------|#

(push (make-instance 'model :mdValue 1) (kids *f1*))

#| observe:
0> The values of the kids are (1)
0> The sum of the values of the kids is 1

;----------------------------------------------------------|#

(push (make-instance 'model :mdValue 2) (kids *f1*))

#| observe:
0> The values of the kids are (2 1)
0> The sum of the values of the kids is 3

;----------------------------------------------------------|#

(setf (kids *f1*) nil)

#| observe:
0> The values of the kids are NIL
0> The sum of the values of the kids is 0

Now before closing, it occurs to me you'll need a little
introduction to the semantics of ^SLOT-X macros generated
by the DEFMODEL macro. Here is another way to define our stone:

|#

(setq *s2* (to-be (make-instance 'stone
                    :accel 2
                    :time-elapsed (cv 3)
                    :distance (c? (+ (^accel) (^time-elapsed))))))

#| In the olden days of Cells, when they were called
Semaphors, the only way to establish a dependency
was to use some form like:

   (^some-slot some-thing)

That is no longer necessary. Now any dynamic access:

(1) during evaluation of a form wrapped in (c?...)
(2) to a cell, direct or inside some function
(3) using accessors named in the defmodel form (not SLOT-VALUE)

...establishes a dependency. So why still have the ^slot macros?

One neat thing about the ^slot macros is that the default
argument is SELF, an anaphor set up by C? and its ilk, so
one can make many rules a little easier to follow by simply
coding (^slot). Another is convenient specification of
Synapses on dependencies, a more advanced topic we can
ignore a while.


|#
