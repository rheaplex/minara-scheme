;; window.scm : windows for minara
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
;; Windows (frames)
;; Documents are attached to a single window at the moment.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Records
(use-modules (srfi srfi-9))
(use-modules (ttn write-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define $window-width 600)
(define $window-height 400)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The list of windows

(define *windows* (make-hash-table 31))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convert a GLUT window ID to a minra window structure

(define (window-for-id id)
  (hash-ref *windows* id))

;; The window record

(define-record-type window
  (really-make-window id
		      buffers
		      width
		      height
		      status
		      undo-stack)
  window?
  ;; The window
  (id window-id)
  ;; The buffers
  (buffers window-buffers
	   set-window-buffers!)
  ;; The window's width
  (width window-width
	  %set-window-width!)
  ;; The window's height
  (height window-height
	  %set-window-height!)
  ;; The window's status string
  (status window-status
	  set-window-status!)
  ;; The window's undo stack
  (undos window-undo-stack
	 set-window-undo-stack!))

;; Set the title

(define (set-window-title! window title)
  (window-set-title (window-id window) title))

;; Utility constructor

(define (%make-window)
  ;; Window must be made before any display lists (MacOSX)!
  (let ((window
	 (really-make-window 
	  (window-make $window-width $window-height)
	  '()
	  -1
	  -1
	  ""
	  '())))
    (hash-create-handle! *windows* (window-id window) window)
    ;; Redraw timestamp
    (initialise-timestamp! window)

    ;; Buffers *under* the main buffer, created bottom to top
    ;; Ask before adding anything here
    (window-view-buffer-make window)
    
    ;; Return the window
    window))

;; Public constructors

(define %untitled-window-name "Untitled")

(define (make-window)
  (let* ((win (%make-window)))
    ;; Main buffer
    (make-window-buffer win "_main")
    (window-undo-stack-push win
			    (window-buffer-main win))
    ;; Set title
    (set-window-title! win %untitled-window-name)
    win))

(define (make-window-from-file file-path)
  (let* ((win (%make-window)))
    ;; Main buffer from file
    (add-window-buffer win "_main" (make-buffer-from-file file-path))
    (window-undo-stack-push win
			    (window-buffer-main win))
    ;; Set buffer text timestamp to file timestamp so it's unchanged for saving
    (timestamp-from-file (buffer-text (window-buffer-main win)) 
			 (buffer-file (window-buffer-main win)))
    ;; Set title
    (set-window-title! win (window-name-base win))
    win))

;; Get the window filename

(define (window-name-base win)
  (let* ((buf (window-buffer-main win))
	 (buf-path (object-property buf 'filename)))
    (if (not buf-path)
	%untitled-window-name
	(basename file-path ".minara"))))

;; Set a window title bar information

(define (set-window-title-info win info)
  (window-set-title (append (window-name-base (window-for-id win)) 
			    "[" info "]")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get window buffer

(define (window-buffer window name)
  (assoc-ref (window-buffers window)
	     name))

;; Get main window buffer

(define (window-buffer-main window)
  (window-buffer window "_main"))

;; Add window buffer
;; We add buffers to the end of the list so we draw newer buffers last
;; This means doing our own assoc-set equivalent

(define (add-window-buffer window buffer-name buffer)
  (if (not (assoc-ref buffer-name
		      buffer))
      (set-window-buffers! window 
			   (append (window-buffers window) 
				   (list (cons buffer-name buffer))))))

(define (make-window-buffer window buffer-name)
  (let ((buf (make-buffer)))
    (add-window-buffer window buffer-name buf)
    buf))

;; Remove window buffer

(define (remove-window-buffer window name)
  (set-window-buffers! window 
		       (assoc-remove! (window-buffers window) 
				      name)))

;; Get the main buffer path

(define (window-buffer-path window)
    (buffer-file (window-buffer-main window)))

;; Reload the document buffer

(define (reload-window-buffer window)
    (let* ((main (window-buffer-main window))
	   (path (window-buffer-path window))
	   (main-timestamp (timestamp (buffer-text main)))
	   (path-timestamp (stat:mtime (stat path))))
      (if (< main-timestamp
	     path-timestamp)
	  (begin
	   (buffer-file-reload main)
	   (window-redraw (window-id window))))))
;;(window-status-temporary window 
;;			   "Buffer changed, not reloaded!" 
;;		   2))))

(define (reload-current-window)
    (reload-window-buffer (window-for-id (window-current))))
 
(keymap-add-fun %global-keymap reload-current-window "x" "r")

     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;; Flag a window as needing redrawing

(define (window-redraw win)
  (window-invalidate win))

;; Draw the window, rebuilding buffer caches as needed

(define (window-draw cb)
  ;; Check timestamps, short circuiting on the first earlier than the window
  ;; If the window cache is more recent than the buffer timestamps
    (install-window-rendering-protocol)
    (push-matrix)
    (for-each
     (lambda (buf-cons)
       (draw-buffer (cdr buf-cons)))
     (window-buffers cb))
    (pop-matrix)
    (update-timestamp! cb))

;; Draw or redraw a window's buffers/caches

(define (window-redraw-event window-id)
    (let ((window (hash-ref *windows* window-id)))
      (if (not (equal? window #f))
	  (begin
	   (window-draw-begin window-id)
	   (window-draw window)
	   ;; Should be set status, like set title. But needs faking in GLUT...
	   (window-draw-status window-id 
			       (string-append %current-tool-name 
					      " "
					      (window-status window)))
	   (window-draw-end window-id)))))

(add-draw-hook window-redraw-event)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Find whether a buffer has ever been saved

(define (buffer-has-file? buf)
  (if (not  (object-property file-buffer 'filename))
      #f
      #t))

;; Find whether a buffer has changed

(define (buffer-changed-since-load? buf)
  (let ((file-path (object-property buf 'filename)))
    (if (not file-path)
	#f
	  (if (> (timestamp buf)
		 (stat:mtime (stat file-path)))
	      #t
	      #f))))

;; Get the file path for a window

;; Save a window buffer, updating the timestamp

(define (save-window win)
  (let* ((file-buffer (buffer-text (window-buffer-main win)))
	 (file-path (object-property file-buffer 'filename)))
    (if (not file-path)
	(error "Trying to save buffer with no associated file")
	(if (buffer-changed-since-load? file-buffer)
	    (let ((backup-path (string-append file-path "~")))
	      (copy-file file-path backup-path)
	      (write-buffer file-buffer file-path))))))

;; Save the current frontmost window

(define (save-current-window)
  (save-window (window-for-id (window-current))))

;; Register keys for saving a window

(keymap-add-fun %global-keymap save-current-window "x" "s")

;; Close a window safely, prompting user and saving if changed

;(define (close-window-event win)
;  (save-window win))

;; Ask the user if they want to close the window, and if so whether to save

;(define (prompt-user-if-changed win)
  ;; IMPLEMENT ME
;  'just-close)

;; Close the frontmost window

;(define (close-current-window)
;  (let ((current-window ((window-for-id (window-current)))))
;    (case (prompt-user-if-changed)
;      ('save-and-close (save-window current-window)
;		       (close-window current-window))
;      ('just-close 
;       (close-window current-window)))))

;; Register keys for closing a window

;(keymap-add-fun %global-keymap close-current-window "x" "c")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window External editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Edit the window's main buffer in an external editor

(define %$default-external-edit-command "/usr/bin/open -a /Applications/TextEdit.app")

(define $external-edit-command %$default-external-edit-command)

(define (external-edit-window window)
    ;; TODO:  Warn user if unsaved!!!!!
    (save-window win)
    (system (string-append $external-edit-command
			   " "
			   (window-buffer-path window)
			   " &")))

;; Edit the current frontmost window

(define (external-edit-current-window)
  (external-edit-window (window-for-id (window-current))))

;; Register keys for editing a window

(keymap-add-fun %global-keymap external-edit-current-window "x" "e")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window status line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the status temporarily
;; Argh, relies on threads which may not have been compiled in.

(define (window-status-temporary window status duration)
    (let ((old-status (window-status window)))
      (set-window-status! window
			  status)
      (window-redraw (window-id window))
      (begin-thread
	  (lambda ()
	    (sleep duration)
	    (if (string= status
			 (window-status window))
		(begin
		 (set-window-status! window 
				     old-status)
		 (window-redraw (window-id window))))))))
