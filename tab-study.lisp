(in-package :common-lisp-user)

(defpackage :tab-study
  (:use :clim-lisp :clim-tab-layout)
  (:export :tab-study))

(in-package :tab-study)

(clim:define-application-frame tab-study ()
  ()
  (:panes
   (a :text-editor :value "This is pane A!")
   (b :text-editor :value "This is pane B!")
   (c :text-editor :value "This is pane C!"))
  (:layouts
   (default
    (clim:vertically ()
      (with-tab-layout ('tab-page :name 'tab-study-layout :height 200)
	("A" a)
	("B" b)
	("C" c))))))

(defun run ()
  (clim:run-frame-top-level (clim:make-application-frame 'tab-study)))

(run)
