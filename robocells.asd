;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

(in-package :asdf)

#+(or allegro lispworks cmu mcl cormanlisp sbcl scl)

(defsystem robocells
  :name "robocells"
  :author "Kenny Tilton <ktilton@nyc.rr.com>"
  :version "1.0.3"
  :maintainer "Kenny Tilton <ktilton@nyc.rr.com>"
  :licence "MIT Style"
  :description "Starter RoboCup client"
  :long-description "A basic RoboCup client with world model and task execution engine"
  :components
   ((:module :cells
     :components (
                  (:file "cells")
                  (:file "flow-control" :depends-on ("cells"))
                  (:file "strings" :depends-on ("flow-control"))
                  (:file "detritus" :depends-on ("flow-control"))
                  (:file "cell-types" :depends-on ("cells"))
                  (:file "debug" :depends-on ("cells"))
                  (:file "initialize" :depends-on ("debug"))
                  (:file "dataflow-management" :depends-on ("debug"))
                  (:file "md-slot-value" :depends-on ("debug"))
                  (:file "calc-n-set" :depends-on ("debug"))
                  (:file "slot-utilities" :depends-on ("debug"))
                  (:file "optimization" :depends-on ("debug"))
                  (:file "link" :depends-on ("debug"))
                  (:file "propagate" :depends-on ("debug"))
                  (:file "synapse" :depends-on ("debug" "cell-types"))
                  (:file "model-object" :depends-on ("debug"))
                  (:file "defmodel" :depends-on ("model-object"))
                  (:file "md-utilities" :depends-on ("defmodel"))
                  (:file "family" :depends-on ("propagate" "model-object" "defmodel"))
                  (:file "fm-utilities" :depends-on ("family"))
                  (:file "family-values" :depends-on ("fm-utilities"))
                  ))

    (:module :robocup
      :components (
                   (:file "robocup")
                   (:file "do-list")
                   (:file "utilities")
                   (:file "tk-geometry")
                   (:file "rc-socket" :depends-on ("robocup"))
                   (:file "server")
                   (:file "sensory")
                   (:file "orientation" :depends-on ("server" "sensory"))
                   (:file "team" :depends-on ("rc-socket"))
                   (:file "task" :depends-on ("rc-socket" "utilities" "sensory"))
                   (:file "soccerbot" :depends-on ("utilities"))
                   (:file "player" :depends-on ("task" "soccerbot"))
                   (:file "trainer" :depends-on ("task" "soccerbot"))
                   )
      :depends-on ("cells"))
    (:file "trainer-task" :depends-on ("robocup"))
    (:file "moving-about" :depends-on ("robocup"))
    (:file "goalie" :depends-on ("robocup"))
    (:file "forward" :depends-on ("robocup"))
    (:file "test" :depends-on ("robocup"))
    ))
