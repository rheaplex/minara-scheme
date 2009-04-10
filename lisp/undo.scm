;; undo.scm : undo and redo support for minara
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
;; Undo/redo
;; Memoization-based.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (minara undo)
  :use-module (srfi srfi-9)
  :use-module (ice-9 gap-buffer)
  :use-module (minara buffer)
  :use-module (minara keymap)
  :use-module (minara window)
  :export (buffer-undo-stack-mark
	   buffer-undo-mark
	   buffer-variable-set-undoable
	   buffer-insert-undoable
	   buffer-delete-undoable
	   window-undo-stack-push
	   window-undo-stack-pop))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window-level undo handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set a buffer as the current undo target: user calls for undo will target it

(define (window-undo-stack-push window buffer)
    (set-window-undo-stack! window
			    (cons buffer 
				  (window-undo-stack window))))

;; Set the undo target to the previous one (possibly none)

(define (window-undo-stack-pop window)
    (set-window-undo-stack! window
			    (cdr (window-undo-stack window))))

;; Ask the window to get its current undo target to undo

(define (window-undo window)
    (let ((current-undo-buffer (car (window-undo-stack window))))
      (if current-undo-buffer
	  (buffer-undo current-undo-buffer))))

;; Ask the window to get its current undo target to redo

(define (window-redo window)
    (let ((current-undo-buffer  (car (window-undo-stack window))))
      (if current-undo-buffer
	  (buffer-redo current-undo-buffer))))

;; Make the undo stack for the window when the window is created

(define (window-undo-stack-install win)  
  (window-undo-stack-push win
			  (window-buffer-main win)))

(add-window-post-main-buffer-hook window-undo-stack-install)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-level undo handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The undo record
;; A linked list node containing an undo and redo method for the action

(define-record-type undoer
    (really-make-undoer undo
			redo
			next
			previous)
  undoer?
  ;; The undo function
  (undo undoer-undo)
  ;; The redo function
  (redo undoer-redo)
  ;; The next undoer
  (next undoer-next
	set-undoer-next!)
  ;; The previous undoer
  (previous undoer-previous
	    set-undoer-previous!))

;; Make an undo record

(define (undoer-make undo redo previous next)
    (really-make-undoer undo
			redo
			next
			previous))

;; Get the buffer undo stack

(define (buffer-undo-stack buffer)
    (let ((stack (buffer-variable buffer 
				  "%undo-stack")))
      (if stack ;; Lazy create stack. Uh. Not sure about this: #f???
	  stack
	  (begin
	   (set-buffer-variable! buffer 
				 "%undo-stack"
				 '())
	   (buffer-undo-mark buffer) ;; Make sure there's a mark at the bottom
	   (buffer-variable buffer 
			    "%undo-stack")))))

(define (set-buffer-undo-stack! buffer new-stack)
    (set-buffer-variable! buffer 
			  "%undo-stack"
			  new-stack))

;; Undo the buffer until we hit the next mark or the end of the undo list

(define (buffer-undo buffer)
    (let* ((undos (buffer-undo-stack buffer)) ;; Get mark at top of stack
	   (previous (undoer-previous undos))) ;; Get previous undo (or nil)
      (if (undoer? previous) ;; If we are not at the base of the undo stack
	  (buffer-undo-aux buffer ;; Move past the mark and start undoing
			   previous))))

(define (buffer-undo-aux buffer undos)
    (let ((undo (undoer-undo undos))
	  (previous (undoer-previous undos)))
      (if undo ;; If we haven't hit the next mark down
	  (begin
	   (undo) ;; Call the undo action
	   (buffer-undo-aux buffer ;; Then recurse
			    previous))
	  (begin ;; If there isn't an undo it's a mark
	   (set-buffer-undo-stack! buffer ;; So set current to mark
				   undos)
	   (buffer-invalidate buffer)))))

;; Redo the buffer until we hit the next mark or the beginning of the undo list

(define (buffer-redo buffer)
    (let* ((undos (buffer-undo-stack buffer)) ;; Get the redos
	   (redos (undoer-next undos))) ;; Get redo after current undo
      (if (undoer? redos) ;; If it exists
	  (buffer-redo-aux buffer 
			   redos))))

(define (buffer-redo-aux buffer redos)
    (let ((redo (undoer-redo redos))
	  (next (undoer-next redos)))
      (if redo ;; If we haven't hit the next mark up
	  (begin
	   (redo) ;; Call the redo action
	   (buffer-redo-aux buffer ;; Then recurse
			    next))
	  (begin ;; If there isn't a redo it's a mark
	   (set-buffer-undo-stack! buffer ;; So set current to mark
				   redos)
	   (buffer-invalidate buffer)))))

;; Push the undo/redo methods & double link them into the undo list

(define (buffer-undo-stack-push buffer undo redo)
    (let* ((current-undo (buffer-undo-stack buffer))
	   (new-undo (undoer-make undo 
				  redo 
				  current-undo
				  #f)))
      (if (undoer? current-undo)
	  (set-undoer-next! current-undo new-undo))
      (set-buffer-undo-stack! buffer
			      new-undo)))



;; Placed at the bottom of / before a series of events 
;; When we undo to this, it makes the *next* item the current undo & stop
;; When we redo to it, it makes the *previous* item the current undo & stop

(define (buffer-undo-mark buffer)
    (buffer-undo-stack-push buffer
			    #f
			    #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer undoable actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set a variable in an undoable way

(define (buffer-variable-set-undoable buffer variable-name value)
    (let ((old-value (buffer-variable buffer variable-name)))
      (buffer-undo-stack-push buffer
			      (lambda (undos)
				(set-buffer-variable! buffer 
						      variable-name
						      old-value))
			      (lambda ()
				(set-buffer-variable! buffer 
						      variable-name 
						      value))))
  (set-buffer-variable! buffer variable-name value))

;; Insert text into a buffer.

(define (buffer-insert-undoable buffer pos text)
    (let* ((gap-buffer (buffer-text buffer))
	   (position (or pos 
			 (gb-point-max gap-buffer)))
	   (text-length (string-length text)))
      (buffer-undo-stack-push buffer
			      (lambda ()
				(gb-goto-char gap-buffer
					      position)
				(gb-delete-char! gap-buffer 
						 text-length))
			      (lambda ()
				(gb-goto-char gap-buffer 
					      position)
				(gb-insert-string! gap-buffer 
						   text)))
      (gb-goto-char gap-buffer 
		    position)
      (gb-insert-string! gap-buffer 
			 text)))

;; Delete text from a buffer

(define (buffer-delete-undoable buffer pos len)
    (let* ((gap-buffer (buffer-text buffer))
	   (position (or pos 
			 (gb-point-min gap-buffer)))
	   (text-length (or len 
			    (gb-point-max gap-buffer)))
	   (deleted-text (gb->substring gap-buffer
					position
					text-length)))
      (buffer-undo-stack-push buffer
			      (lambda ()
				(gb-goto-char gap-buffer 
					      position)
				(gb-insert-string! gap-buffer 
						   deleted-text))
			      (lambda ()
				(gb-goto-char gap-buffer
					      position)
				(gb-delete-char! gap-buffer 
						 text-length)))
      (gb-goto-char gap-buffer 
		    position)
      (gb-delete-char! gap-buffer 
		       text-length)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (call-window-undo)
    (let* ((win (window-current)))
      (window-undo win)
      (window-redraw win)))

(keymap-add-fun-global call-window-undo "z")

(define (call-window-redo)
    (let* ((win (window-current)))
      (window-redo win)
      (window-redraw win)))

(keymap-add-fun-global call-window-redo "Z")
