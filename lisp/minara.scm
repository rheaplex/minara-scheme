;; minara.scm : minara scheme development file
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


;; This is the work-in-progress file. 
;; Code will be moved into other files as it matures.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; getopt-long...
(use-modules (ice-9 getopt-long))

;; define-record-type
(use-modules (srfi srfi-9))

;; Matrices, Colour
;;(use-modules (ice-9 slib))

;; Buffers
(use-modules (ttn gap-buffer))
(use-modules (ttn find-file))

;; Line reading
(use-modules (ice-9 rdelim))

;; Rendering
(use-modules ((rendering)
              :renamer (symbol-prefix-proc 'rendering:)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The top-level bindings for the protocol functions

(define write-header #f)
(define write-footer #f)
(define set-colour #f)
(define path-begin #f)
(define path-end #f)
(define move-to #f)
(define line-to #f)
(define curve-to #f)

;; Install the render protocol

(define (install-render-protocol)
  (set! set-colour rendering:set-colour)
  (set! path-begin rendering:path-begin)
  (set! path-end rendering:path-end)
  (set! move-to rendering:move-to)
  (set! line-to rendering:line-to)
  (set! curve-to rendering:curve-to))

;; Macro to save/restore current protocol


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
  (install-render-protocol)
  (eval-string (gb->string buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cached Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The buffer record

(define-record-type cached-buffer
  (really-make-cached-buffer cache buffer cache-timestamp buffer-timestamp)
  cached-buffer?
  ;; The draw cache
  (cache cached-buffer-cache)
  ;; The text buffer
  (buffer cached-buffer-buffer)
  ;; The timestamp for each
  (buffer-timestamp cached-buffer-buffer-timestamp 
		    set-cached-buffer-buffer-timestamp!)
  (cache-timestamp cached-buffer-cache-timestamp 
		   set-cached-buffer-cache-timestamp!))

;; Public constructor

(define (make-cached-buffer)
  (really-make-cached-buffer (cache-make)
			     (make-gap-buffer)
			     0
			     (current-time)))

;; Load the buffer from file

(define (make-cached-buffer-from-file file-path)
  (really-make-cached-buffer (cache-make)			     
			     (find-file file-path)
			     0
			     (current-time)))

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
				      save-timestamp)
  cached-buffered-window?
  ;; The buffer and draw cache
  (cached-buffer cached-buffered-window-cached-buffer)
  ;; The window
  (window cached-buffered-window-window)
  ;; The file path for the window
  (file-path cached-buffered-window-file-path 
	     set-cached-buffered-window-file-path!)
  ;; The save timestamp. This may interact with undo...
  (save-timestamp cached-buffered-window-save-timestamp))
;; Tool & HUD buffer or buffer list?

;; Public constructors

(define (make-cached-buffered-window)
	 ;; Window must be made before any display lists (MacOSX)!
  (let* ((win (window-make))
	 (window
	   (really-make-cached-buffered-window 
	    (make-cached-buffer) win "" (current-time))))
    (hash-create-handle! *windows* win window)
    window))

(define (make-cached-buffered-window-from-file file-path)
  ;; Window must be made before any display lists (MacOSX)!
  (let* ((win (window-make))
	 (window (really-make-cached-buffered-window 
	    (make-cached-buffer-from-file file-path) 
	    win file-path (current-time))))
    (hash-create-handle! *windows* win window)
    window))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The tool record


;; The tool invocation context record
;; Tool instance (session?): tool, window and tool work cached-buffer


;; Declare a tool

;; Get the current tool

;; Install a tool

;; Remove a tool



;; Suspend undo
;; Resume undo
;; with-undo-disabled macro

;; Append tool buffer to file buffer


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the global application configuration file

(define (load-config)
  (if (access? "./minara-config.scm" R_OK)
      (load "./minara-config.scm")))

;; Load the user's ~/.minara file

(define (load-user-config)
  (if (access? "~/.minara" R_OK)
      (load "~/.minara")))

;; Load the libraries

(define (load-libraries)
;;  (define dir (opendir "/usr/lib"))
;;  (do ((entry (readdir dir) (readdir dir)))
;;      ((eof-object? entry))
;;    (display entry)(newline))
;;  (closedir dir))
  #f)

;; Load the tools

(define (load-tools)
  #f)

;; Load the splash screen if required

(define (load-splash-screen)
  (make-cached-buffered-window-from-file "../minara.minara"))

;; Our main startup

(define (startup args)
  (debug-enable 'debug)
  (debug-enable 'backtrace)
  (let* ((option-spec '((file (value #t))))
         (options (getopt-long args option-spec))
	 (from-file (option-ref options 'file #f)))
    ;; Load the application configuration file
    (load-config)
    ;; Load the user config file
    (load-user-config)
    ;; Load the libraries
    (load-libraries)
    ;; Load the tools
    (load-tools)
    ;; Get the event hooks back into C in case anyone forgot to
    (bind-event-hooks)
    ;; Load the splash screen if no file, otherwise load file
    ;; Important, as GLUT crashes if started without a window!
    (if (equal? from-file #f)
	(load-splash-screen)
       	(make-cached-buffered-window-from-file from-file))))
