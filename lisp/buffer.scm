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
;; Discussion
;;
;; We have several layers of buffers.
;;
;; A buffer is a gap buffer from the ttn library. This is just an Emacs-style
;; gap buffer. I'm told this is the most efficient kind of text buffer.
;;
;; A cached buffer is a gap buffer with the graphical result of evaluating the
;; buffer cached in some useful form. So for OpenGL this might be a display
;; list, for some libraries this might be an rgba pixmap, for others it might
;; be some GPU-side representation. The idea is that if the buffer has
;; not been changed, it is quicker to use the cached representation rather
;; than to re-evaluate the text of the buffer.
;; Obviously this doesn't work if the buffer's content is affected by the time
;; at which it is evaluated. This needs some consideration.
;;
;; A cached buffered window is a cached buffer attached to a window, possibly
;; with a number of other cached buffers in a list above it. These other
;; buffers are used for overlays or storing work in progress by tools or
;; actions. They can be quickly disposed of or added to the main buffer when
;; work in them has finished.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO
;; Make buffer variables their own object, get from buff or multi

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
;; Timestamps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Timestamps

(define (timestamp buf)
  (object-property buf 'timestamp))

(define (initialise-timestamp! buf)
  (set-object-property! buf 'timestamp 0))

(define (update-timestamp! buf)
  (set-object-property! buf 'timestamp (current-time)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cached Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The buffer record

(define-record-type cached-buffer
  (really-make-cached-buffer cached-buffer-cache 
			     cached-buffer-buffer 
			     buffer-variables)
  cached-buffer?
  ;; The draw cache
  (cached-buffer-cache get-cache)
  ;; The text buffer
  (cached-buffer-buffer get-buffer)
  ;; The variables alist
  (buffer-variables buffer-variables
		    set-buffer-variables!))

;; Public constructor

(define (make-cached-buffer)
  (let ((buf (really-make-cached-buffer (cache-make)
					(make-gap-buffer)
					'())))
    (update-timestamp! (get-buffer buf))
    (update-timestamp! (get-cache buf))
    buf))

;; Set buffer variable

(define (set-buffer-variable! buffer name value)
  (set-buffer-variables! buffer 
			 (assoc-set! (buffer-variables buffer) 
				     name value)))

;; Get buffer variable

(define (buffer-variable buffer name value)
  (assoc (buffer-variables buffer) name value))

;; Remove buffer variable

(define (kill-buffer-variable! buffer name)
  (set-buffer-variables! buffer 
			 (assoc-remove! (buffer-variables buffer) 
					name)))

;; Remove all buffer variables

(define (kill-all-buffer-variables buffer)
  (set-buffer-variables! '()))

;; Load the buffer from file

(define (make-cached-buffer-from-file file-path)
  (let ((buf (really-make-cached-buffer (cache-make)			     
					(find-file file-path)
					'())))
    (update-timestamp! (get-buffer buf))
    (initialise-timestamp! (get-cache buf))
    buf))

;; Redraw the buffer (just run the cache if no timestamp variation)

(define (draw-cached-buffer cb)
  ;; Cache more recent than buffer?
  (if (< (timestamp (get-buffer cb))
	 (timestamp (get-cache cb)))
      ;; Just redraw
     (begin
       (cache-draw (get-cache cb)))
      ;; Otherwise, generate the cache and update the timestamp
      (begin 
	(install-window-rendering-protocol)
	(let ((c (get-cache cb)))
	  (cache-record-begin c)
	  (eval-string (gb->string (get-buffer cb)))
	  (cache-record-end c)
	  (update-timestamp! (get-cache cb))))))

;; Set the buffer as ready to redraw

(define (cached-buffer-invalidate cb)
  (update-timestamp! (get-buffer cb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cached Multiple Buffers
;; Combine the multi/buffer variable accessors 
;; so we don't need to know the type?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The multi-buffer record

(define-record-type cached-multi-buffer
  (really-make-cached-multi-buffer multi-cache 
				   multi-buffers 
				   multi-buffer-variables)
  cached-buffer?
  ;; The draw cache
  (multi-cache get-multi-cache)
  ;; The text buffers
  (multi-buffers get-multi-buffers
		 set-multi-buffers!)
  ;; The variables alist
  (multi-buffer-variables multi-buffer-variables
			  set-multi-buffer-variables!))

;; Public constructor

(define (make-cached-multi-buffer)
  (let ((buf (really-make-cached-multi-buffer (cache-make)
					'()
					'())))
    (update-timestamp! (get-multi-buffers buf))
    (update-timestamp! (get-multi-cache buf))
    buf))

;; Get a single named buffer from the multi-buffer

(define (get-multi-buffer multi-buf name)
  (cdr (assoc name
	 (get-multi-buffers multi-buf))))

;; Add a named buffer to the multi-buffer

(define (add-multi-buffer multi-buf buffer-name)
  (set-multi-buffers! 
   multi-buf
   (assoc-set! (get-multi-buffers multi-buf)
	       buffer-name 
	       (make-gap-buffer)))
  (get-multi-buffer multi-buf buffer-name))

;; Remove named multi-bbuffer

(define (remove-multi-buffer multi-buff name)
  (set-multi-buffers!
   multi-buff 
   (assoc-remove! (get-multi-buffers multi-buff) 
		  name)))

;; Remove all window overlays

(define (kill-all-buffers multi-buff)
  (set-multi-buffers! '()))

;; Set buffer variable

(define (set-multi-buffer-variable! buffer name value)
  (set-multi-buffer-variables! buffer 
			 (assoc-set! (multi-buffer-variables buffer) 
				     name value)))

;; Get buffer variable

(define (multi-buffer-variable buffer name value)
  (assoc (multi-buffer-variables buffer) name value))

;; Remove buffer variable

(define (kill-multi-buffer-variable! buffer name)
  (set-multi-buffer-variables! buffer 
			 (assoc-remove! (multi-buffer-variables buffer) 
					name)))

;; Remove all buffer variables

(define (kill-all-multi-buffer-variables buffer)
  (set-multi-buffer-variables! '()))

;; Check the timestamp on each buffer.
;; If any one is newer than the cache timestamp,
;; then redraw them all.

(define (draw-cached-multi-buffer cb)
  ;; Cache more recent than buffer?
  (if (< (timestamp (get-multi-buffers cb))
	 (timestamp (get-multi-cache cb)))
      ;; Just redraw the cache
     (begin
	(cache-draw (get-multi-cache cb)))
      ;; Otherwise, generate the cache and update the timestamp
      (begin 
	(install-window-rendering-protocol)
	(let ((c (get-multi-cache cb)))
	  (cache-record-begin c)
	  (for-each
	   (lambda (buf)
	     (eval-string (gb->string (cdr buf))))
	   (get-multi-buffers cb))
	  (cache-record-end c)
	  (update-timestamp! (get-multi-cache cb))))))
	
;; Set the buffer as ready to redraw

(define (multi-buffer-invalidate cb)
  (update-timestamp! (get-multi-buffers cb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cached Buffered Window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The list of windows

(define *windows* (make-hash-table 31))

;; The cached buffered window record

(define-record-type window
  (really-make-window cached-buffer 
		      window-id
		      overlay-buffers
		      height)
  window?
  ;; The buffer and draw cache
  (cached-buffer window-cached-buffer)
  ;; The window
  (window-id window-id)
  ;; Tool/HUD/overlay buffers alist
  (overlay-buffers window-overlays
		   set-window-overlays!)
  ;; The window's height
  (height window-height
	  %set-window-height!))


;; Public constructors

(define (make-window)
  ;; Window must be made before any display lists (MacOSX)!
  (let* ((win (window-make))
	 (window
	  (really-make-window 
	    (make-cached-buffer) 
	    win 
	    (make-cached-multi-buffer)
	    0)))
    (hash-create-handle! *windows* win window)
    (update-timestamp! win)
    window))

(define (make-window-from-file file-path)
  ;; Window must be made before any display lists (MacOSX)!
  (let* ((win (window-make))
	 (window 
	  (really-make-window 
	   (make-cached-buffer-from-file file-path) 
	   win 
	   (make-cached-multi-buffer)
	   0)))
    (hash-create-handle! *windows* win window)
    (update-timestamp! win)
    window))

;; Get window overlay cached buffer

(define (window-overlay window name)
  (get-multi-buffer (window-overlays window) name))

;; Add an overlay

(define (add-overlay window name)
  (add-multi-buffer (window-overlays window) name))

;; Get window overlay buffer

(define (overlay-buffer window name)
  (get-multi-buffer (window-overlays window) name))

;; Remove window overlay

(define (remove-overlay window name)
  (remove-multi-buffer (window-overlays window) name))

;; Remove all window overlays

(define (kill-all-overlays window)
  (kill-all-buffers (window-overlays window)))

;; Just get the buffer

(define (window-for-id id)
  (hash-ref *windows* id))

(define (window-cached-buffer-for-id id)
   (window-cached-buffer 
    (window-for-id id)))

(define (window-buffer-for-id id)
  (get-buffer 
   (window-cached-buffer-for-id id)))

;; Draw or redraw a window's buffers/caches

(define (window-redraw window)
  (draw-cached-buffer (window-cached-buffer window))
  (draw-cached-multi-buffer (window-overlays window)))

(define (window-redraw-event window-id)
  (let ((window (hash-ref *windows* window-id)))
    (if (not (equal? window #f))
	(window-redraw window))))

(add-draw-hook window-redraw-event)


;; Save a window buffer, updating the timestamp

;; Close a window safely, saving if changed
