(in-package :common-lisp-user)

(defpackage :presentation-types
  (:use :clim :clim-lisp)
  (:export :app-main))

(in-package :presentation-types)

(define-application-frame superapp ()
  ()
  (:pointer-documentation t)
  (:panes
   (app :application
	:display-time t
	:height 300
	:width 600)
   (int :interactor
	:height 200
	:width 600))
  (:layouts
   (default (vertically ()
	      app
	      int))))

(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp)))

(define-superapp-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-presentation-type name-of-month ()
  :inherit-from 'string)

(define-presentation-type day-of-month ()
  :inherit-from 'integer)

;; print two objects to *standard-output*
(define-superapp-command (com-out :name t) ()

  ;; associate the presentation type 'name-of-month with the underlying object: "The third month"
  (with-output-as-presentation (t "The third month" 'name-of-month)
    (format t "March~%"))

  ;; associate the presentation tye 'day-of-month with the underlying object: 15
  (with-output-as-presentation(t 15 'day-of-month)
    (format t "fifteen~%")))

(define-superapp-command (com-get-date :name t)
    ((name 'name-of-month) (date 'day-of-month))
  (format (frame-standard-input *application-frame*)
	  "the ~a of ~a~%" date name))

(app-main)
