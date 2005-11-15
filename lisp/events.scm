;; events.scm : gui event handling
;;
;; Copyright (c) 2004 Rob Myers, rob@robmyers.org
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic GUI and application events
;;
;; We receive events from the window system via the C code, which may have
;; done some setting up of the environment for us.
;;
;; We keep a list of handlers for each event, and call each in turn.
;;
;; People really *shouldn't* replace these functions, instead they should
;; use them to add and remove event hooks within the system set up here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calling event handlers with good error recovery and diagnostics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-last-stack #f)


;;(define (call-with-backtrace call-me)
;;(call-me))

(define (call-with-backtrace call-me)
    (set! the-last-stack 
	  (make-stack #t)) 
  (catch #t
    call-me
    event-error-handler)
  (set! the-last-stack #f))

(define (event-error-handler . args)
    (if (= (length args) 5)
	(begin
	 (apply display-error 
		#f 
		(current-error-port) 
		(cdr args))
	 (display-backtrace the-last-stack
			    (current-error-port)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quitting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %quit-funs '())

(define (%quit-hook)
    (call-with-backtrace
     (lambda ()
       (for-each (lambda (fun)
		   (fun))
		 %quit-funs))
     event-error-handler))

(define (add-quit-hook fun)
    (if (not (memq fun 
		   %quit-funs))
	(set! %quit-funs 
	      (cons fun 
		    %quit-funs))))

(define (remove-quit-hook fun)
    (set! %quit-funs
	  (delq fun 
		%quit-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resizing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %resize-funs '())

(define (%resize-hook win width height)
    (let ((window (window-for-id win)))
      (call-with-backtrace
       (lambda ()
	 (for-each (lambda (fun)
		     (fun window 
			  width 
			  height))
		   %resize-funs)))))

(define (add-resize-hook fun)
    (if (not (memq fun 
		   %resize-funs))
	(set! %resize-funs 
	      (cons fun 
		    %resize-funs))))

(define (remove-resize-hook fun)
    (set! %resize-funs
	  (delq fun 
		%resize-funs)))

;; GLUT's window co-ords go down, OGL's go up.
;; So we need to allow for this

(define (%update-window-dimensions window width height)
    (%set-window-width! window 
			width)
  (%set-window-height! window 
		       height))

(add-resize-hook %update-window-dimensions)

(define (%swizzle-y win y)
    (- (window-height win)
       y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %draw-funs '())

(define (%draw-hook win)
    (let ((window (window-for-id win)))
      (call-with-backtrace
       (lambda ()
	 (for-each (lambda (fun)
		     (fun window))
		   %draw-funs)))))

(define (add-draw-hook fun)
    (if (not (memq fun 
		   %draw-funs))
	(set! %draw-funs 
	      (cons fun 
		    %draw-funs))))

(define (remove-draw-hook fun)
    (set! %draw-funs
	  (delq fun 
		%draw-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse down
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %mouse-down-funs '())

(define (%mouse-down-hook win button x y)
    (let ((window (window-for-id win)))
      (call-with-backtrace
       (lambda ()
	 (let ((yy (%swizzle-y window 
			       y)))
	   (for-each (lambda (fun)
		       (fun window 
			    button 
			    x 
			    yy))
		     %mouse-down-funs))))))

(define (add-mouse-down-hook fun)
    (if (not (memq fun 
		   %mouse-down-funs))
	(set! %mouse-down-funs 
	      (cons fun 
		    %mouse-down-funs))))

(define (remove-mouse-down-hook fun)
    (set! %mouse-down-funs
	  (delq fun 
		%mouse-down-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %mouse-up-funs '())

(define (%mouse-up-hook win button x y)
    (let ((window (window-for-id win)))
      (call-with-backtrace
       (lambda ()
	 (let ((yy (%swizzle-y window 
			       y)))
	   (for-each (lambda (fun)
		       (fun window 
			    button 
			    x 
			    yy))
		     %mouse-up-funs))))))

(define (add-mouse-up-hook fun)
    (if (not (memq fun 
		   %mouse-up-funs))
	(set! %mouse-up-funs 
	      (cons fun 
		    %mouse-up-funs))))

(define (remove-mouse-up-hook fun)
    (set! %mouse-up-funs
	  (delq fun 
		%mouse-up-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %mouse-move-funs '())

(define (%mouse-move-hook win x y)
    (let ((window (window-for-id win)))
      (call-with-backtrace
       (lambda ()
	 (let ((yy (%swizzle-y window 
			       y)))
	   (for-each (lambda (fun)
		       (fun window 
			    x 
			    yy))
		     %mouse-move-funs))))))

(define (add-mouse-move-hook fun)
    (if (not (memq fun 
		   %mouse-move-funs))
	(set! %mouse-move-funs 
	      (cons fun 
		    %mouse-move-funs))))

(define (remove-mouse-move-hook fun)
    (set! %mouse-move-funs
	  (delq fun 
		%mouse-move-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key presses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %key-press-funs '())

(define (%key-press-hook win key modifiers)
    (let ((window (window-for-id win)))
      (call-with-backtrace
       (lambda ()
	 (for-each (lambda (fun)
		     (fun window 
			  key 
			  modifiers))
		   %key-press-funs)))))

(define (add-key-press-hook fun)
    (if (not (memq fun 
		   %key-press-funs))
	(set! %key-press-funs 
	      (cons fun 
		    %key-press-funs))))

(define (remove-key-press-hook fun)
    (set! %key-press-funs 
	  (delq fun 
		%key-press-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key releases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %key-release-funs '())

(define (%key-release-hook win key modifiers)
    (let ((window (window-for-id win)))
      (call-with-backtrace
       (lambda ()
	 (for-each (lambda (fun)
		     (fun window 
			  key 
			  modifiers))
		   %key-release-funs)))))

(define (add-key-release-hook fun)
    (if (not (memq fun 
		   %key-release-funs))
	(set! %key-release-funs 
	      (cons fun 
		    %key-release-funs))))

(define (remove-key-release-hook fun)
    (set! %key-release-funs 
	  (delq fun 
		%key-release-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu Selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %menu-select-funs '())

(define (%menu-select-hook win menu-id)
    (let ((window (window-for-id win)))
      (call-with-backtrace
       (lambda ()
	 (for-each (lambda (fun)
		     (fun window 
			  menu-id))
		   %menu-select-funs)))))

(define (add-menu-select-hook fun)
    (if (not (memq fun 
		   %menu-select-funs))
	(set! %menu-select-funs 
	      (cons fun 
		    %menu-select-funs))))

(define (remove-menu-select-hook fun)
    (set! %menu-select-funs
	  (delq fun 
		%menu-select-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make these event handlers accessible to the C code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-event-hooks)