;; -*- lisp-version: "6.1 [Windows] (Apr 8, 2003 17:12)"; common-graphics: "1.389.2.67.2.24"; -*-

(in-package :common-graphics-user)

(defpackage :cells (:export))

(define-project :name 'cells
  :application-type (intern "Standard EXE" (find-package :keyword))
  :modules (list (make-instance 'module :name "cells\\cells.lisp")
                 (make-instance 'module :name "cells\\debug.lisp")
                 (make-instance 'module :name "cells\\trc.lisp")
                 (make-instance 'module :name "cells\\initialize.lisp")
                 (make-instance 'module :name
                                "cells\\dataflow-management.lisp")
                 (make-instance 'module :name
                                "cells\\md-slot-value.lisp")
                 (make-instance 'module :name "cells\\calc-n-set.lisp")
                 (make-instance 'module :name
                                "cells\\slot-utilities.lisp")
                 (make-instance 'module :name
                                "cells\\optimization.lisp")
                 (make-instance 'module :name "cells\\link.lisp")
                 (make-instance 'module :name "cells\\propagate.lisp")
                 (make-instance 'module :name "cells\\synapse.lisp")
                 (make-instance 'module :name
                                "cells\\model-object.lisp")
                 (make-instance 'module :name "cells\\defmodel.lisp")
                 (make-instance 'module :name
                                "cells\\md-utilities.lisp")
                 (make-instance 'module :name "cells\\family.lisp")
                 (make-instance 'module :name
                                "cells\\fm-utilities.lisp")
                 (make-instance 'module :name
                                "cells\\family-values.lisp")
                 (make-instance 'module :name
                                "cells\\strudel-object.lisp"))
  :projects (list (make-instance 'project-module :name "cl-kenny"))
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
