;;; A study of the VIEW and PRESENTATION-METHOD concepts in McCLIM.
;; Taken from the McCLIM manual.

(in-package #:common-lisp-user)

(defpackage #:views-study
  (:use #:clim-lisp #:clim)
  (:export #:run-views-study))

(in-package #:views-study)

(defclass person ()
  ((%last-name :initarg :last-name :accessor last-name)
   (%first-name :initarg :first-name :accessor first-name)
   (%address :initarg :address :accessor address)
   (%membership-number :initarg :membership-number :reader membership-number)))

;; person constructor
(defun make-person (last-name first-name address membership-number)
  (make-instance 'person
                 :last-name last-name
                 :first-name first-name
                 :address address
                 :membership-number membership-number))

;; initial list of people
(defparameter *members*
  (list (make-person "Doe" "Jane" "123, Glencoe Terrace" 12345)
        (make-person "Dupont" "Jean" "111, Rue de la Republique" 54321)
        (make-person "Smith" "Eliza" "22, Trafalgar Square" 121212)
        (make-person "Nilsson" "Sven" "Uppsalagatan 33" 98765)))

;; A presentation method that specifies how a person object should be represented by a view.
;; A presentation method can be specialized to a particular view, but this one is specialized to any stream view
(define-presentation-method present ((object person) (type person)
                                     stream view &key)
  (declare (ignore view))

  ;; present a person object as their first and last name separated by a space
  (format stream "~A ~A" (first-name object) (last-name object)))

;; A presentation method that specifies how a stream view should be parsed to a person object.
(define-presentation-method accept ((type person) stream view &key)

  ;; allow spaces
  (with-delimiter-gestures (nil :override t)

    ;; take a string and do not show another prompt
    (let ((name (accept 'string :stream stream :view view
                        :prompt "" :prompt-mode :raw)))

      ;; search for a matching first and last name
      (or (find name *members*
                :test #'string=
                :key #'(lambda (person)
                         (format nil "~A ~A" (first-name person)
                                 (last-name person))))

	  ;; throw an error
          (simple-parse-error "~A isn't in your address book!" name)))))

;;; the CLIM view class that corresponds to a list of members, one member
;;; per line of text in a CLIM application pane.
(defclass members-view (view) ())

;;; since this view does not take any parameters in our simple example,
;;; we need only a single instance of it.
(defparameter *members-view* (make-instance 'members-view))

;; The application frame
(define-application-frame views ()
  ((%members :initform *members* :accessor members))
  (:panes
   (main-pane :application :height 500 :width 500
              :display-function 'display-main-pane
              :default-view *members-view*) ;; the default view for the frame
   (interactor :interactor :height 100 :width 500))
  (:layouts
   (default (vertically ()
              main-pane
              interactor))))

;; A generic display function that is called on the frame, pane, AND view. A standard
;; CLIM display function isn't called on a view.
(defgeneric display-pane-with-view (frame pane view))

;; The display function called during each iteration of the command loop. We specified this
;; function in the DEFINE-APPLICATION-FRAME. We simply call our DISPLAY-PANE-WITH-VIEW with
;; the default view of the pane which we specifed as members-view
(defun display-main-pane (frame pane)
  (display-pane-with-view frame pane (stream-default-view pane)))

;; The actual method that specifies how a list of people should be displayed by members-view
(defmethod display-pane-with-view (frame pane (view members-view))
  (loop for member in (members frame)
        do (with-output-as-presentation
               (pane member 'person)
             (format pane "~a, ~a, ~a, ~a~%"
                     (membership-number member)
                     (last-name member)
                     (first-name member)
                     (address member)))))

;; a view for displaying all information about a single person
(defclass person-view (view)
  ((%person :initarg :person :reader person)))

;; this method on our own display function shows the detailed
;; information of a single member.
(defmethod display-pane-with-view (frame pane (view person-view))
  (let ((person (person view)))
    (format pane "Last name: ~a~%First Name: ~a~%Address: ~a~%Membership Number: ~a~%"
            (last-name person)
            (first-name person)
            (address person)
            (membership-number person))))

(defun run-views-study ()
  (run-frame-top-level (make-application-frame 'views)))

;; Command to quit the application
(define-views-command (com-quit :name t) ()
  (frame-exit *application-frame*))

;; Command to switch the current view of the application pane.
;; This switches the pane to show all members in our address book.
(define-views-command (com-show-all :name t) ()
  (setf (stream-default-view *standard-output*) *members-view*))

;; Command to make the pane display all info for one person.
(define-views-command (com-show-person :name t) ((person 'person))
  (setf (stream-default-view *standard-output*)
        (make-instance 'person-view :person person)))

(run-views-study)
