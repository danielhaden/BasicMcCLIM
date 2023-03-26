(in-package :common-lisp-user)

(defpackage "APP"
  (:use :clim :clim-lisp)
  (:export "app-main"))

(in-package :app)

(define-application-frame stopwatch ()
  ()
  ;; :panes section describes different parts of the
  ;; application-frame. This application has only one
  ;; pane
  (:panes
   (int :interactor :height 400 :width 600))

  ;; :layouts section describers how the panes are layed out.
  ;; This application has one layout named "default" which has
  ;; a single pane.
  (:layouts
   (default int)))

;;; launches an instance of "superapp" application-frame
(defun app-main ()
  (run-frame-top-level (make-application-frame 'stopwatch)))

(app-main)
