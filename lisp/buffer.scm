;; buffer.scm : buffers, graphics and windows for minara
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
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Buffers
(use-modules (ttn gap-buffer))
(use-modules (ttn find-file))

;; Records
(use-modules (srfi srfi-9))

;; Line reading
(use-modules (ice-9 rdelim))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evaluate the contents of the buffer
;; We need one of these each for the draw and pick protocols

(define (evaluate-buffer-draw buf)
;;  (let ((port (make-gap-buffer-port buf)))
;;    (while (char-ready? port)
;;	   (primitive-eval (read-line port)))))
  ;; SLOOOOOOWWWWW!!!! Fix the above code or improve it!
  (install-window-rendering-protocol)
  (eval-string (gb->string buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cached Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The buffer record

(define-record-type cached-buffer
  (really-make-cached-buffer cache buffer cache-timestamp buffer-timestamp 
			     buffer-variables)
  cached-buffer?
  ;; The draw cache
  (cache cached-buffer-cache)
  ;; The text buffer
  (buffer cached-buffer-buffer)
  ;; The timestamp for each
  (buffer-timestamp cached-buffer-buffer-timestamp 
		    set-cached-buffer-buffer-timestamp!)
  (cache-timestamp cached-buffer-cache-timestamp 
		   set-cached-buffer-cache-timestamp!)
  ;; The variables alist
  (buffer-variables cached-buffer-variables
		    set-cached-buffer-variables!))

;; Public constructor

(define (make-cached-buffer)
  (really-make-cached-buffer (cache-make)
			     (make-gap-buffer)
			     0
			     (current-time)
			     '()))

;; Set buffer variable

(define (set-buffer-variable! buffer name value)
  (set-cached-buffer-variables! buffer 
				(assoc-set! (cached-buffer-variables buffer) 
					    name value)))

;; Get buffer variable

(define (buffer-variable buffer name value)
  (assoc (cached-buffer-variables buffer) name value))

;; Remove buffer variable

(define (kill-buffer-variable! buffer name)
  (set-cached-buffer-variables! buffer 
				(assoc-remove! (cached-buffer-variables buffer) 
					    name)))

;; Remove all buffer variables

(define (kill-all-buffer-variables buffer)
  (set-cached-buffer-variables! '()))

;; Load the buffer from file

(define (make-cached-buffer-from-file file-path)
  (really-make-cached-buffer (cache-make)			     
			     (find-file file-path)
			     0
			     (current-time)
			     '()))

;; Redraw the buffer (just run the cache if no timestamp variation)

(define (draw-cached-buffer cb)
  ;; Cache more recent than buffer?
  (if (< (cached-buffer-buffer-timestamp cb)
	 (cached-buffer-cache-timestamp cb))
      ;; Just redraw
     (begin
	(cache-draw (cached-buffer-cache cb)))
      ;; Otherwise, generate the cache and update the timestamp
      (begin (let ((c (cached-buffer-cache cb)))
	  (cache-record-begin c)
	  (evaluate-buffer-draw (cached-buffer-buffer cb))
	  (cache-record-end c)
	  (set-cached-buffer-cache-timestamp! cb (current-time))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cached Buffered Window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The list of windows

(define *windows* (make-hash-table 31))

;; The cached buffered window record

(define-record-type cached-buffered-window
  (really-make-cached-buffered-window cached-buffer 
				      window
				      file-path
				      save-timestamp
				      overlay-buffers)
  cached-buffered-window?
  ;; The buffer and draw cache
  (cached-buffer cached-buffered-window-cached-buffer)
  ;; The window
  (window cached-buffered-window-window)
  ;; The file path for the window
  (file-path cached-buffered-window-file-path 
	     set-cached-buffered-window-file-path!)
  ;; The save timestamp. This may interact with undo...
  (save-timestamp cached-buffered-window-save-timestamp)
  ;; Tool/HUD/overlay buffers alist
  (overlay-buffers cached-buffered-window-overlays
		   set-cached-buffered-window-overlays!))

;; Public constructors

(define (make-cached-buffered-window)
	 ;; Window must be made before any display lists (MacOSX)!
  (let* ((win (window-make))
	 (window
	   (really-make-cached-buffered-window 
	    (make-cached-buffer) win "" (current-time) '())))
    (hash-create-handle! *windows* win window)
    window))

(define (make-cached-buffered-window-from-file file-path)
  ;; Window must be made before any display lists (MacOSX)!
  (let* ((win (window-make))
	 (window (really-make-cached-buffered-window 
	    (make-cached-buffer-from-file file-path) 
	    win file-path (current-time) '())))
    (hash-create-handle! *windows* win window)
    window))

;; Add an overlay

(define (set-cached-buffered-window-overlay! buffer name value)
  (set-cached-buffered-window-overlays! buffer 
				(assoc-set! (cached-buffer-window-overlays 
					     buffer) 
					    name value)))

;; Get window overlay

(define (cached-buffered-window-overlay buffer name value)
  (assoc (cached-buffered-window-overlays buffer) name value))

;; Remove window overlay

(define (kill-buffered-window-overlay! buffer name)
  (set-cached-buffered-window-overlays! buffer 
				(assoc-remove! (cached-buffered-window-overlays
						buffer) 
					    name)))

;; Remove all window overlays

(define (kill-all-buffered-window-overlays buffer)
  (set-cached-buffered-window-overlays! '()))

;; Create and add a buffer

(define (make-cached-buffered-window-overlay buffer name)
  (set-cached-buffered-window-overlay! buffer name (make-cached-buffer)))

;; Draw or redraw a window's buffers/caches

(define (window-redraw window)
  (draw-cached-buffer (cached-buffered-window-cached-buffer window)))

(define (window-redraw-event window-id)
  (let ((window (hash-ref *windows* window-id)))
    (if (not (equal? window #f))
	(window-redraw window))))

(set! %draw-hook window-redraw-event)

;; Save a window buffer, updating the timestamp

;; Close a window safely, saving if changed
