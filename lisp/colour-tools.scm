;; colour-tools.scm : minara scheme development file
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

;; Colour support

(define current-colour "(set-colour 0.0 0.0 0.0 0.0)")

(define (write-current-colour buff)
    (buffer-insert-undoable buff
			    #f
			    (format #f 
				    "~a~%" 
				    current-colour)))

;; RGB

(define rgb-minor-amount 0.01)
(define rgb-major-amount 0.1)

(define rgb-current-r (make-variable 0.0))
(define rgb-current-g (make-variable 0.0))
(define rgb-current-b (make-variable 0.0))
(define rgb-current-a (make-variable 0.0))

(define rgb-current-component #f)

(define (rgb-current-string)
    (format #f 
	    "(set-colour ~f ~f ~f ~f)"
	    (variable-ref rgb-current-r)
	    (variable-ref rgb-current-g)
	    (variable-ref rgb-current-b)
	    (variable-ref rgb-current-a)))

(define (set-rgb-current-colour)
    (set! current-colour
	  (rgb-current-string)))

(define (rgb-set-component component to)
    (cond 
      ((> to
	 1.0)
       (variable-set! component
		      1.0))
      ((< to
	  0.0)
       (variable-set! component
	     0.0))
      (else 
       (variable-set! component
		      to))))

(define (rgb-set-current-component to)
    (if rgb-current-component
	(rgb-set-component rgb-current-component 
			   to)))

(define (rgb-current-status-update window)
    (set-window-status! window
			(format #f 
				"- Red: ~f Green: ~f Blue: ~f Alpha: ~f"
				(variable-ref rgb-current-r)
				(variable-ref rgb-current-g)
				(variable-ref rgb-current-b)
				(variable-ref rgb-current-a))))

(define (rgb-current-buffer-make window) 
    (make-window-buffer window
			"rgb"))

(define (rgb-current-buffer-destroy window) 
    (remove-window-buffer window
			  "rgb"))

(define (rgb-current-buffer window)
    (window-buffer window
		   "rgb"))

(define (rgb-current-sample-update window) 
    (let ((buff (rgb-current-buffer window)))
      (buffer-erase buff)
      (buffer-insert-undoable buff
			      #f
			      (rgb-current-string))
      (buffer-insert-undoable buff
			      #f
			      "(path-begin)\n(move-to 10 40)\n(line-to 10 90)\n (line-to 60 90)\n(line-to 60 40)\n(path-end)")
      (buffer-undo-mark buff)
      (buffer-invalidate buff)))

(define (rgb-current-refresh)
    (let ((window (window-current))) ;; Update for multi-window
      (rgb-current-status-update window)
      (rgb-current-sample-update window)
      (window-redraw window)))

(define (rgb-update-current-component to)
    (rgb-set-current-component to)
  (rgb-current-refresh))

(define (rgb-add-current-component to)
    (if rgb-current-component
	(rgb-set-current-component (+ rgb-current-component
				      to))))

;; The tool's keymap

(define %rgb-keymap (keymap-make))

;; Install/uninstall the tool

(define (rgb-current-tool-install)
    (let ((window (window-current)))
      (rgb-current-buffer-make window)
      (keymap-current-root-set %rgb-keymap)
      (rgb-current-refresh)))

(define (rgb-current-tool-uninstall)
    (let ((window (window-current)))
      (set-rgb-current-colour)
      (keymap-current-root-reset)
      (set-window-status! window
			  "")
      (rgb-current-buffer-destroy window)
      (window-redraw window)))

;; Key presses

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (set! rgb-current-component rgb-current-r))
		"r")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (set! rgb-current-component rgb-current-g))
		"g")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (set! rgb-current-component rgb-current-b))
		"b")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (set! rgb-current-component rgb-current-a))
		"a")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-add-current-component rgb-minor-amount))
		"=")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-add-current-component rgb-major-amount))
		"+")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-add-current-component (- rgb-minor-amount)))
		"-")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-add-current-component (- rgb-major-amount)))
		"_")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-update-current-component 0.0))
		"0")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-update-current-component 0.1))
		"1")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-update-current-component 0.2))
		"2")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-update-current-component 0.3))
		"3")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-update-current-component 0.4))
		"4")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-update-current-component 0.5))
		"5")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-update-current-component 0.6))
		"6")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-update-current-component 0.7))
		"7")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-update-current-component 0.8))
		"8")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-update-current-component 0.9))
		"9")

(keymap-add-fun %rgb-keymap
		(lambda ()
		  (rgb-update-current-component 1.0))
		"!")

(keymap-add-fun %rgb-keymap
		rgb-current-tool-uninstall
		"q")


;; Register

(install-tool rgb-current-tool-install 
	      rgb-current-tool-uninstall
	      "RGB"
	      "c" "r")