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
;; View buffer
;; The window buffer below the main buffer that gives the window view
;; view when evaluated.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (window-view-buffer window)
  (window-buffer window "_view"))

(define (window-view-buffer-current)
  (window-view-buffer (current-window)))

;; This is called in the window constructor. Don't call it in your own code.

(define (window-view-buffer-make window)
  (make-window-buffer window "_view")
  (let ((buffer (window-buffer window
			       "_view")))
    (set-buffer-variable! buffer 
			  "scale" 
			  $zoom-normal)
    (set-buffer-variable! buffer
			  "scale-tx"
			  0.0)
    (set-buffer-variable! buffer
			  "scale-ty"
			  0.0)
    (set-buffer-variable! buffer 
			  "tx" 
			  0.0)
    (set-buffer-variable! buffer 
			  "ty" 
			  0.0)
    (set-buffer-variable! buffer 
			  "pan-tx" 
			  0.0)
    (set-buffer-variable! buffer 
			  "pan-ty" 
			  0.0)
    (set-buffer-variable! buffer 
			  "angle" 
			  0.0)))

(define (view-buffer-update buffer)
 (let ((scale (buffer-scale buffer))
       (text (buffer-text buffer)))
   (gb-erase! text)
    (gb-insert-string!
     text
     (format #f
 	    "(translate ~f ~f)~%"
 	    (buffer-scale-tx buffer)
 	    (buffer-scale-ty buffer)))
   (gb-insert-string!
    text
    (format #f
	    "(translate ~f ~f)~%"
	    (buffer-tx buffer)
	    (buffer-ty buffer)))
   (gb-insert-string!
    text
    (format #f
	    "(translate ~f ~f)~%"
	    (buffer-pan-tx buffer)
	    (buffer-pan-ty buffer)))
   (gb-insert-string!
    text
    (format #f
	    "(rotate ~f)~%"
	    (buffer-rotation buffer)))
   (gb-insert-string!
    text
    (format #f
	    "(scale ~f ~f)"
	    scale
	    scale))
;;   (format #t "~A~%~%" (gb->string text))
 (buffer-invalidate buffer)))

(define (window-view-update window)
  (view-buffer-update (window-view-buffer window))
  (window-redraw (window-id window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zoom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Scale factors and view translation factors.
;; If anyone can explain this to me I'd be very grateful... - Rob.


(define $zoom-max-out 0.0625)
(define $zoom-normal 1.0)
(define $zoom-max-in 16.0)

(define (buffer-scale buffer)
  (buffer-variable buffer
		   "scale"))

(define (window-scale window)
  (buffer-scale (window-buffer window
			       "_view")))

(define (buffer-scale-tx buffer)
  (buffer-variable buffer
		   "scale-tx"))

(define (buffer-scale-ty buffer)
  (buffer-variable buffer
		   "scale-ty"))

(define (window-scale-tx window)
  (buffer-scale-tx (window-buffer window
			       "_view")))

(define (window-scale-ty window)
  (buffer-scale-ty (window-buffer window
			       "_view")))

(define (next-zoom-out-level current)
  (if (= current 
	 $zoom-max-out)
      #f
      (/ current
	 2.0)))

(define (next-zoom-in-level current)
  (if (= current 
	 $zoom-max-in)
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
  (let* ((buffer (window-view-buffer window))
	 (scale (buffer-scale buffer)))
    (set-buffer-variable! buffer
			  "scale-tx"
			  (/ (- (window-width window)
				(* (window-width window)
				   scale))
			     2.0))
    (set-buffer-variable! buffer
			  "scale-ty"
			  (/ (- (window-height window)
				(* (window-height window)
				   scale))
			     2.0)))
  (window-view-update window))

(define (zoom-mouse-up win button x y)
  ;; Ultimately zoom & pan with same click here
  (let* ((window (window-for-id win))
	 (buffer (window-view-buffer window))
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
  (buffer-pan-zoom-factor (window-view-buffer window)))

(define (buffer-tx buffer)
  (buffer-variable buffer 
		   "tx")) 

(define (buffer-ty buffer)
  (buffer-variable buffer 
		   "ty")) 

(define (set-buffer-tx! buffer tx)
  (set-buffer-variable! buffer 
			"tx"
			tx)) 

(define (set-buffer-ty! buffer ty)
  (set-buffer-variable! buffer 
			"ty"
			ty)) 

;; The temporary translate used by the pan tool.
;; Rename

(define (buffer-pan-tx buffer)
  (buffer-variable buffer 
		   "pan-tx")) 

(define (buffer-pan-ty buffer)
  (buffer-variable buffer 
		   "pan-ty")) 

(define (set-buffer-pan-tx! buffer tx)
  (set-buffer-variable! buffer 
		   "pan-tx"
		   tx)) 

(define (set-buffer-pan-ty! buffer ty)
  (set-buffer-variable! buffer 
		   "pan-ty"
		   ty)) 

(define (window-tx window)
  (buffer-tx (window-view-buffer window)))

(define (window-ty window)
  (buffer-ty (window-view-buffer window)))

(define (set-window-transform window)
  (let ((buffer (window-view-buffer window)))
    (set-buffer-tx! buffer 
		    (+ (buffer-pan-tx buffer)
		       (buffer-tx buffer)))
    (set-buffer-ty! buffer
		    (+ (buffer-pan-ty buffer)
		       (buffer-ty buffer)))
    (set-buffer-pan-tx! buffer 0.0)
    (set-buffer-pan-ty! buffer 0.0)))

(define pan-tool-mouse-down #f)
(define pan-tool-mousedown-x #f)
(define pan-tool-mousedown-y #f)

(define (pan-mouse-down win button x y) 
  (set! pan-tool-mouse-down #t)
  (let ((window (window-for-id win)))
    (set! pan-tool-mousedown-x x)
    (set! pan-tool-mousedown-y y)))

(define (pan-mouse-move win x y) 
  (if pan-tool-mouse-down
      (let* ((window (window-for-id win))
	     (buffer (window-view-buffer window)))
	(set-buffer-pan-tx! buffer
			    (- x;;(window-view-x window x)
			       pan-tool-mousedown-x))
	(set-buffer-pan-ty! buffer
			    (- y;;(window-view-y window y)
			       pan-tool-mousedown-y))
	(window-view-update window))))

(define (pan-mouse-up win button x y) 
  (set! pan-tool-mouse-down #f)
  (let ((window (window-for-id win)))
    (set-window-transform window)
    (window-view-update window)))

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
  (/ (+ (window-tx window)
     (window-scale-tx window))
	(- (window-scale window))))

(define (window-view-right window)
  (+ (window-view-width window)
     (window-tx window)))

(define (window-view-bottom window)
  (/ (+ (window-ty window)
	(window-scale-ty window))
     (- (window-scale window))))

(define (window-view-top window)
  (+ (window-view-height window)
     (window-ty window)))

(define (window-view-x window x)
  (+ (/ x
	(window-scale window))
     (window-view-left window)))
  
  
(define (window-view-y window y)
  (+ (/ (- y
	   (- (window-height window) ;; Cache this on window resize
	      $window-height))
	(window-scale window))
     (window-view-bottom window)))
	
