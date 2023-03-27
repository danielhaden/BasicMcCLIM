(in-package :common-lisp-user)

(defpackage :app-frame-basic
  (:use :clim :clim-lisp)

  (:export :run-basic-app))

(in-package :app-frame-basic)

(define-application-frame basic-app()
  () ;; for additional superclasses

     ;; slots would go here (e.g. application specific data)

  (:pointer-documentation t) ;; provides documentation for different actions on the pointer device
                             ;; 
 
  (:panes ;; a two pane application

   ;; main application for user interaction
   (app :application
	:display-time nil ;; specifies when the pane should be displayed in the command loop
        :height 400       ;; the default option is :command-loop which means the pane is cleared
        :width 600)       ;; after each iteration of the command loop and redisplayed using a client
                          ;; supplied display function. With :display-time nil, the pane is never
                          ;; cleared and output is accumulated every time we execute a command
   ;; a CLI
   (int :interactor
	:height 400
	:width 600))

  (:layouts ;; specifies how the panes should be arranged
   (default (vertically () ;; default layout specifes they are stacked vertically
	      app int))))

;;; a command to be invoked when the application exits. Note that
;;; the -basic-app- part refers to the name of the application-frame.
;;; to be available in the CLI, commands must have the :name t.
(define-basic-app-command (com-quit :name t) ()
  (frame-exit *application-frame*))

;;; a command that determines the parity of an integer
(define-basic-app-command (com-parity :name t) ((number 'integer))
  (format t "~a is ~a~%" number
	  (if (oddp number)
	      "odd"
	      "even")))

;;; call to run the application
(defun run-basic-app ()
  (run-frame-top-level (make-application-frame 'basic-app)))

(run-basic-app)
