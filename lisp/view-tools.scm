;; view-tools.scm : tools for viewing windows for minara
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
;; view-tools
;; Tools for changing window views.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (ice-9 format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transform buffer
;; The window buffer below the main buffer that gives the window view
;; transform when evaluated.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (window-transform-buffer window)
  (let ((buf (window-buffer window "transform-buffer")))
    (if buf
	buf
	(window-transform-buffer-make window))))

(define (window-transform-buffer-current)
  (window-transform-buffer (current-window)))

(define (window-transform-buffer-make window)
  (make-window-buffer window "transform-buffer")
  (let ((buffer (window-buffer window
			       "transform-buffer")))
    (set-buffer-variable! buffer 
			  "zoom-level" 
			  1.0)
    (set-buffer-variable! buffer 
			  "pan-offset" 
			  (cons 0.0 0.0))
    (set-buffer-variable! buffer 
			  "tilt-rotation" 
			  0.0)
    buffer))

(define (transform-buffer-update buffer)
  (gb-erase! buffer)
 (let ((translation (buffer-translation buffer))
       (scale (buffer-scale buffer)))
   (gb-insert-string!
    (buffer-text buffer)
    (format #f
	    "(rotate ~f)~%(scale ~f ~f)~%(translate ~f ~f)"
	    (buffer-rotation buffer)
	    scale
	    scale
	    (car translation)
	    (cdr translation))))
 (buffer-invalidate buffer))

(define (window-transform-update window)
  (transform-buffer-update (window-transform-buffer window))
  (window-redraw (window-id window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zoom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %zoom-max-out 0.001)
(define %zoom-max-in 1024.0)

(define (buffer-scale buffer)
  (buffer-variable buffer
		   "zoom-level"))

(define (next-zoom-out-level current)
  (if (= current 
	 %zoom-max-out)
      #f
      (* current
	 2.0)))

(define (next-zoom-in-level current)
  (if (= current 
	 %zoom-max-in)
      #f
      (* current
	 2.0)))

(define (next-zoom-level current in?)
  (if in?
      (next-zoom-in-level current)
      (next-zoom-out-level current)))

(define (buffer-zoom-update buffer zoom)
  (set-buffer-variable! buffer
			"zoom-level"
			zoom))

(define (window-zoom-update window zoom)
  (buffer-zoom-update (window-buffer window
				     "transform-buffer") 
		      zoom)
  (window-transform-update window))

(define (zoom-mouse-up win button x y) 
  ;; Ultimately zoom & pan with same click here
  (let* ((window (window-for-id win))
	 (buffer (window-transform-buffer window))
	 (current-zoom (buffer-scale buffer))
	 (zoom (next-zoom-level current-zoom 
				(= button
				   1))))
    (if zoom
	(window-zoom-update window
			    zoom))))

;; Install

(define (zoom-tool-install)
  (add-mouse-up-hook zoom-mouse-up))

;; Uninstall

(define (zoom-tool-uninstall)
  (remove-mouse-up-hook zoom-mouse-up))

;; Register

(install-tool zoom-tool-install 
	      zoom-tool-uninstall
	      "Zoom"
	      "t" "s")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pan-zoom-factor zoom)
  (/ 1.0 zoom))

(define (buffer-pan-zoom-factor buffer)
  (pan-zoom-factor (buffer-scale buffer)))

(define (window-pan-zoom-factor window)
  (buffer-pan-zoom-factor (window-transform-buffer window)))

(define (buffer-translation buffer)
  (buffer-variable buffer 
		   "pan-offset")) 

(define (window-pan-offset window)
  (buffer-pan-offset (window-transform-buffer window)))

(define (set-pan-offset buffer x y)
  (set-buffer-variable! buffer 
			"pan-offset" 
			(cons x y)))

(define (set-pan-from-delta buffer x y)
  (let* ((coords (buffer-translation buffer))
	 (old-x (car coords))
	 (old-y (cdr coords))
	 (new-x (+ old-x
		   x))
	 (new-y (+ old-y
		   y)))
    (set-pan-offset buffer
		    new-x
		    new-y)))

(define (set-pan-from-delta-zoomed buffer x y)
  (let ((zoom (buffer-pan-zoom-factor buffer)))
    (set-pan-from-delta buffer
			(* x zoom)
			(* y zoom))))

(define (window-pan-update-delta window x y)
  (set-pan-from-delta-zoomed (window-transform-buffer window) x y)
  (window-transform-update window))

(define pan-tool-mouse-down #f)
(define pan-tool-previous-x #f)
(define pan-tool-previous-y #f)

(define (pan-mouse-down win button x y) 
  (set! pan-tool-mouse-down #t)
  (set! pan-tool-previous-x x)
  (set! pan-tool-previous-y y))

(define (pan-mouse-move win x y) 
  (if pan-tool-mouse-down
      (window-pan-update-delta (window-for-id win)
			       (- x
				  pan-tool-previous-x)
			       (- y
				  pan-tool-previous-y))))

(define (pan-mouse-up win button x y) 
  (set! pan-tool-mouse-down #f))

;; Install

(define (pan-tool-install)
  (add-mouse-down-hook pan-mouse-down)
  (add-mouse-move-hook pan-mouse-move)
  (add-mouse-up-hook pan-mouse-up))

;; Uninstall

(define (pan-tool-uninstall)
  (remove-mouse-down-hook pan-mouse-down)
  (remove-mouse-move-hook pan-mouse-move)
  (remove-mouse-up-hook pan-mouse-up))

;; Register

(install-tool pan-tool-install 
	      pan-tool-uninstall
	      "Pan"
	      "t" "t")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tilt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (buffer-rotation buffer)
  (buffer-variable buffer
		   "tilt-rotation")) 