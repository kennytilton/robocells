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

(in-package :robocup)

#| --- ASAP -----

world model

skills

check MIT license is in all source

|#

#| --- develeopment ------------

try again on starting server from lisp

|#

#| --- lowlevel stuff -----------


|#

#| --- performance --------------

memoize landpoint location?

actually, a lot of calculations of things like interception will involve
repeated calculations over the same parameters. esp. if we get into fuzzy
math, memoization becomes practical.

things like "relative to me, where  and in what direction are the goal posts?" 
can be known directly from one's body position and dir. this maps nicely onto
the reality of someone having a feel for their environment.

|#

#| --- game play ---------------------

going offside by overrunning the ball on otherwise legal breakaways, i think.

|#

#| --- subtasks ---------------------------

goalie

dribbling, including turn-ball

passing

training/testing

stamina management

scoring (play around goal)

defending (play around goal)

|#

#| --- framework ---------------

there is a pattern with stuff like our-side-p, which is a macro
only to capture self. how's about a new defun-selfish which expands
into `(defun call-<whatever> (self) ,@body)

have one-time tasks just go away once achieved: echo achieved to drop
kid from task (kids) list if a "one-time-p flag is set.

|#

#| --- cells ---------------

dataflow interference handling loses *cause* info for debugging

when ruled ephemerals (something new) get optimized away, do not get reset to nil, probably 
because they are not found by md-slot-cell

also, shouldn't ephemerals reset the slot value as well as the value slot of the cell?
certainly so the inspector shows the right thing for debugging

dataflow management has a problem: ephemerals get reset to nil before deferred cells 
get revisited. possibly this can be handled by having *deferred* (making that up)
rebound at each causal node, so they get revisited before the ephemeral unwinds.

ruled cells can memoize after a calculation by going back to dependencies and 
stashing the calculated value keyed off a list of each dependencies value.

ugh. un-echoed trainer slot never got access by anyone, so no trainer got made. this
could be a runtime warning, or we could awaken all cells echo or no, with an option
to suppress for oddball cases like the debug slot. or also solve this non-kid model
issue by declaring a slot to be model-bearing, and then having the internals both
call to-be on it and fill in a "host" slot (or generalize .fmParent to be that?)

A cyclic dependency sent s-deep-stale-p into stack overflow

usage counting from end is weird, plus c-unlink-unused does weird mvb of
what looks to be unnecessary "unlinked" value

|#
