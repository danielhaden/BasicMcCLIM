(ql:quickload 'clim-user)

(in-package #:clim-user)

(define-presentation-method present (object (type t)
					    (stream)
					    (view textual-view)
					    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format stream "System: -s." object))

(defun display (frame stream)
  (draw-rectangle* stream 10 10 40 40 :ink (test-frame-color frame))
  (present (asdf:find-system "mcclim")))

(define-application-frame test-frame ()
  ((my-color :initarg :color :initform +darkred+))
  (:panes (app :application)
	  (int :interactor))
  (:geometry :width 600 :height 480)
  (:layout (default (vertically ()
		      app int))))

(run-frame-top-level (make-application-frame 'test-frame))
