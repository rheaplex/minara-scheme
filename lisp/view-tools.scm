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
  (window-buffer window "_view"))

(define (window-transform-buffer-current)
  (window-transform-buffer (current-window)))

;; This is called in the window constructor. Don't call it in your own code.

(define (window-transform-buffer-make window)
  (make-window-buffer window "_view")
  (let ((buffer (window-buffer window
			       "_view")))
    (set-buffer-variable! buffer 
			  "scale" 
			  1.0)
    (set-buffer-variable! buffer 
			  "tx" 
			  0.0)
    (set-buffer-variable! buffer 
			  "ty" 
			  0.0)
    (set-buffer-variable! buffer 
			  "angle" 
			  0.0)))

(define (transform-buffer-update buffer)
 (let ((scale (buffer-scale buffer))
       (text (buffer-text buffer)))
   (gb-erase! text)
   (gb-insert-string!
    text
    (format #f
	    "(translate ~f ~f)~%(rotate ~f)~%(scale ~f ~f)"
	    (buffer-tx buffer)
	    (buffer-ty buffer)
	    (buffer-rotation buffer)
	    scale
	    scale)))
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
		   "scale"))

(define (window-scale window)
  (buffer-scale (window-buffer window
			       "_view")))

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
			"scale"
			zoom))

(define (window-zoom-update window zoom)
  (buffer-zoom-update (window-buffer window
				     "_view") 
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
  (set-current-tool-name! "Zoom")
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

(define (buffer-tx buffer)
  (buffer-variable buffer 
		   "tx")) 

(define (buffer-ty buffer)
  (buffer-variable buffer 
		   "ty")) 

(define (window-tx window)
  (buffer-tx (window-transform-buffer window)))

(define (window-ty window)
  (buffer-ty (window-transform-buffer window)))

(define (set-pan-offset buffer x y)
  (set-buffer-variable! buffer 
			"tx" 
			x)
  (set-buffer-variable! buffer 
			"ty" 
			y))

(define (set-pan-from-delta buffer x y)
  (set-pan-offset buffer
		  (+ x
		     pan-tool-old-buffer-x)
		  (+ y
		     pan-tool-old-buffer-y)))

(define (set-pan-from-delta-zoomed buffer x y)
  (let ((zoom (buffer-pan-zoom-factor buffer)))
    (set-pan-from-delta buffer
			(* x zoom)
			(* y zoom))))

(define (window-pan-update-delta window x y)
  (set-pan-from-delta-zoomed (window-transform-buffer window) 
			     x 
			     y)
  (window-transform-update window))

(define pan-tool-mouse-down #f)
(define pan-tool-old-buffer-x #f)
(define pan-tool-old-buffer-y #f)
(define pan-tool-mousedown-x #f)
(define pan-tool-mousedown-y #f)

(define (pan-mouse-down win button x y) 
  (set! pan-tool-mouse-down #t)
  (let ((window (window-for-id win)))
    (set! pan-tool-old-buffer-x (window-tx window))
    (set! pan-tool-old-buffer-y (window-ty window)))
  (set! pan-tool-mousedown-x x)
  (set! pan-tool-mousedown-y y))

(define (pan-mouse-move win x y) 
  (if pan-tool-mouse-down
      (window-pan-update-delta (window-for-id win)
			       (- x
				  pan-tool-mousedown-x)
			       (- y
				  pan-tool-mousedown-y))))

(define (pan-mouse-up win button x y) 
  (set! pan-tool-mouse-down #f))

;; Install

(define (pan-tool-install)
  (add-mouse-down-hook pan-mouse-down)
  (add-mouse-move-hook pan-mouse-move)
  (add-mouse-up-hook pan-mouse-up))

;; Uninstall

(define (pan-tool-uninstall)
  (set-current-tool-name! "Pan")
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
		   "angle")) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View/window co-ordinate conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (window-view-width window)
  (/ (window-width window)
     (window-scale window)))

(define (window-view-height window)
  (/ (window-height window)
     (window-scale window)))

(define (window-view-left window)
  (window-tx window))

(define (window-view-right window)
  (+ (window-view-width window)
     (window-tx window)))

(define (window-view-bottom window)
  (window-ty window))

(define (window-view-top window)
  (+ (window-view-height window)
     (window-ty window)))

(define (window-view-x window x)
  (/ (- x
	(window-tx window))
     (window-scale window)))

(define (window-view-y window y)
  (/ (- (- y
	   (- (window-height window) ;; Cache this on window resize
	      $window-height))
	(window-ty window))
     (window-scale window)))