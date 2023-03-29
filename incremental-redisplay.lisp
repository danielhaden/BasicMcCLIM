(in-package :common-lisp-user)

(defpackage :incremental-redisplay-app
  (:use :clim :clim-lisp)
  (:export app-main))

(in-package :incremental-redisplay-app)

(define-application-frame superapp ()
  ;; SLOTS
  ;; a list of 20 singleton lists each containing a random number
  ((numbers :initform (loop repeat 20 collect (list (random 1000000000)))
	    :accessor numbers)

   ;; the line number the cursor is on
   (cursor :initform 0
	   :accessor cursor))

  ;; provides documentation for different commands on the device pointed to
  (:pointer-documentation t)

   (:panes
    (app :application
	 :height 400 :width 600
	 :incremental-redisplay t
	 :display-function 'display-app)  ;; the display function to be invoked in the command loop
    (int :interactor :height 200 :width 600))
   (:layouts
    (default (vertically () app int))))

;; specifies how to display the pane, not the app
(defun display-app (frame pane)

  (loop
    ;; for each random number in the slot numbers
    for current-element in (numbers frame)

    for line from 0

    ;; print a "*" to the line the cursor is on. This will simply be printed
    ;; at each call of 'display-app. There is no incremental redisplay
    do (princ (if (= (cursor frame) line) "*" " ") pane)

    ;; perform incremental redisplay. That is, for each line in the pane, see if the number changed
    ;; before redrawing the element   
    do (updating-output (pane :unique-id   current-element ;; a singleton list, if the number itself
			                                   ;; were used, two identical numbers could
			                                   ;; not be distinguished
			      
			      :id-test     #'eq  ;; specifies the predicate to determine
			                         ;; if the element is the same as the one
			                         ;; that was issued last time
			      
			      :cache-value (car current-element) ;; the previous value that the element had
			      :cache-test  #'eql) ;; the predicate to determine if the element's value changed
	 (format pane "~a~%" (car current-element)))))

;; Add the specifed amount to the number marked by "*"
(define-superapp-command (com-add :name t) ((number 'integer))
  (incf (car (elt (numbers *application-frame*)
		  (cursor *application-frame*)))
	number))

;; Move the "*" to the next line
(define-superapp-command (com-next :name t) ()
  (incf (cursor *application-frame*))

  ;; when on the last line, set the cursor to 0
  (when (= (cursor *application-frame*)
           (length (numbers *application-frame*)))
    (setf (cursor *application-frame*) 0)))

;; Move the "*" to the previous line
(define-superapp-command (com-previous :name t) ()
  (decf (cursor *application-frame*))

  ;; when on the first line, set the cursor to the last line
  (when (minusp (cursor *application-frame*)) ;; is the cursor line <0?
    (setf (cursor *application-frame*)
	  (1- (length (numbers *application-frame*))))))

;; Quit the 
(define-superapp-command (com-quit :name t) ()
  (frame-exit *application-frame*))

;; Invoke to run the app
(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp)))

(app-main)
