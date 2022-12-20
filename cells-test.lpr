;; -*- lisp-version: "6.1 [Windows] (Apr 8, 2003 17:12)"; common-graphics: "1.389.2.67.2.24"; -*-

(in-package :common-graphics-user)

(defpackage :cells (:export))

(define-project :name 'celltest
  :application-type (intern "Standard EXE" (find-package :keyword))
  :modules (list (make-instance 'module :name "cellstest\\test.lisp")
                 (make-instance 'module :name
                                "cellstest\\hello-world.lisp")
                 (make-instance 'module :name
                                "cellstest\\internal-combustion.lisp")
                 (make-instance 'module :name
                                "cellstest\\boiler-examples.lisp")
                 (make-instance 'module :name "cellstest\\person.lisp")
                 (make-instance 'module :name
                                "cellstest\\df-interference.lisp")
                 (make-instance 'module :name
                                "cellstest\\test-family.lisp")
                 (make-instance 'module :name
                                "cellstest\\test-kid-slotting.lisp"))
  :projects (list (make-instance 'project-module :name "cells"))
  :libraries nil
  :distributed-files nil
  :project-package-name :cells
  :main-form nil
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:aclwin302 :carets :cg :choose-list :color-dialog
                     :common-control :common-status-bar :dde
                     :directory-list :drag-and-drop :edit-in-place
                     :find-dialog :font-dialog :grid :header-control
                     :hotspots :group-box :lisp-widget
                     :list-view-control :mci :menu-selection
                     :multi-picture-button :ole :outline
                     :progress-indicator-control :string-dialog
                     :tab-control :trackbar-control :up-down-control
                     :yes-no-list-dialog)
  :help-file-module (make-instance 'build-module :name "")
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:compiler :source-file-info :top-level :xref-info)
  :build-flags '(:suppress-systray-icon :us-government-use :allow-debug
                 :exit-after-build)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cm +t \"Initializing\""
  :old-space-size 2000000
  :new-space-size 2000000
  :runtime-build-option :standard
  :setcmd-path nil
  :on-initialization 'cells::cv-test
  :on-restart 'do-default-restart)

;; End of Project Definition
