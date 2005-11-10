;; shape-tools.scm : minara scheme development file
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
;; Circle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define circle-tool-mouse-down #f)
(define circle-tool-mouse-down-x #f)
(define circle-tool-mouse-down-y #f)
  
;; When the button is pressed, make and initialise the circle drawing buffer

(define (circle-mouse-down win button x y) 
    (let* ((window (window-for-id win))
	   (circle-buffer (buffer-text (make-window-buffer window
							   "circle"))))
    (window-undo-stack-push window
			    circle-buffer)
    (set! circle-tool-mouse-down #t)
    (set! circle-tool-mouse-down-x (window-view-x window
						  x))
    (set! circle-tool-mouse-down-y (window-view-y window
						  y))))

(define (circle-mouse-move win raw-x raw-y) 
    (if circle-tool-mouse-down
	(let* ((window (window-for-id win))
	       (circle-buffer (window-buffer window
					     "circle"))
	       (x (window-view-x window 
				 raw-x))
	       (y (window-view-y window
				 raw-y))
	       (radius (abs (distance-between-points circle-tool-mouse-down-x
						     circle-tool-mouse-down-y
						     x
						     y)))
	       (left (- circle-tool-mouse-down-x radius))
	       (right (+ circle-tool-mouse-down-x radius))
	       (top (+ circle-tool-mouse-down-y radius))
	       (bottom (- circle-tool-mouse-down-y radius)))
	  (gb-erase! (buffer-text circle-buffer))
	  (buffer-insert-undoable circle-buffer 
				  #f
				  "(path-begin)\n")
	  (buffer-insert-undoable circle-buffer 
				  #f
				  (format #f 
					  "(move-to ~f ~f)~%"
					  left y))
	  (buffer-insert-undoable circle-buffer 
				  #f
				  (format #f 
					  "(curve-to ~f ~f ~f ~f ~f ~f)~%"
					  left top
					  left top
					  x top))
	  (buffer-insert-undoable circle-buffer 
				  #f
				  (format #f 
					  "(curve-to ~f ~f ~f ~f ~f ~f)~%"
					  right top
					  right top
					  right y))
	  (buffer-insert-undoable circle-buffer 
				  #f
				  (format #f 
					  "(curve-to ~f ~f ~f ~f ~f ~f)~%"
					  right bottom
					  right bottom
					  x bottom))
	  (buffer-insert-undoable circle-buffer 
				  #f
				  (format #f 
					  "(curve-to ~f ~f ~f ~f ~f ~f)~%"
					  left bottom
					  left bottom
					  left y))
	  (buffer-insert-undoable circle-buffer 
				  #f
				  "(path-end)\n")
	  (buffer-undo-mark circle-buffer)
	  (buffer-invalidate circle-buffer)   
	  (window-redraw win))))

;; When the mouse is released, add the circle  buffer to the main buffer
;; Redraw the main buffer, and release the pen drawing buffers

(define (circle-mouse-up win button x y) 
  (set! circle-tool-mouse-down #f)
  (let* ((window (window-for-id win))
	 (buff (buffer-text (window-buffer
			     window
			    "circle")))
	(main-buff (window-buffer-main window)))
    (buffer-insert-undoable main-buff
			    #f
			    (gb->string buff))
    (buffer-undo-mark main-buff)
    (window-undo-stack-pop window)
    ;; Clean up and redraw
    (remove-window-buffer window 
			  "circle")
    (buffer-invalidate (window-buffer-main window))
    (window-redraw win)))

;; Install

(define (circle-tool-install)
  (add-mouse-move-hook circle-mouse-move)
  (add-mouse-down-hook circle-mouse-down)
  (add-mouse-up-hook circle-mouse-up))

;; Uninstall

(define (circle-tool-uninstall)
  (remove-mouse-move-hook circle-mouse-move)
  (remove-mouse-down-hook circle-mouse-down)
  (remove-mouse-up-hook circle-mouse-up))

;; Register

(install-tool circle-tool-install 
	      circle-tool-uninstall
	      "Circle"
	      "t" "c")

;; Oval

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Square
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define square-tool-mouse-down #f)
(define square-tool-mouse-down-x #f)
(define square-tool-mouse-down-y #f)
  
;; When the button is pressed, make and initialise the drawing buffer

(define (square-mouse-down win button x y) 
    (let* ((window (window-for-id win))
	   (buff (buffer-text (make-window-buffer window
						  "square"))))
    (window-undo-stack-push window
			    buff)
    (set! square-tool-mouse-down #t)
    (set! square-tool-mouse-down-x (window-view-x window
						  x))
    (set! square-tool-mouse-down-y (window-view-y window
						  y))))

(define (square-mouse-move win raw-x raw-y) 
    (if square-tool-mouse-down
	(let* ((window (window-for-id win))
	       (buff (window-buffer window
				    "square"))
	       (x (window-view-x window 
				 raw-x))
	       (y (window-view-y window
				 raw-y))
	       (radius (abs (distance-between-points square-tool-mouse-down-x
						     square-tool-mouse-down-y
						     x
						     y)))
	       (left (- square-tool-mouse-down-x radius))
	       (right (+ square-tool-mouse-down-x radius))
	       (top (+ square-tool-mouse-down-y radius))
	       (bottom (- square-tool-mouse-down-y radius)))
	  (gb-erase! (buffer-text buff))
	  (buffer-insert-undoable buff 
				  #f
				  "(path-begin)\n")
	  (buffer-insert-undoable buff
				  #f
				  (format #f 
					  "(move-to ~f ~f)~%"
					  left bottom))
	  (buffer-insert-undoable buff
				  #f
				  (format #f 
					  "(line-to ~f ~f)~%"
					  left top))
	  (buffer-insert-undoable buff
				  #f
				  (format #f 
					  "(line-to ~f ~f)~%"
					  right top))
	  (buffer-insert-undoable buff
				  #f
				  (format #f 
					  "(line-to ~f ~f)~%"
					  right bottom))
	  (buffer-insert-undoable buff 
				  #f
				  (format #f 
					  "(line-to ~f ~f)~%"
					  left bottom))
	  (buffer-insert-undoable buff
				  #f
				  "(path-end)\n")
	  (buffer-undo-mark buff)
	  (buffer-invalidate buff)   
	  (window-redraw win))))

;; When the mouse is released, add the circle  buffer to the main buffer
;; Redraw the main buffer, and release the pen drawing buffers

(define (square-mouse-up win button x y) 
  (set! square-tool-mouse-down #f)
  (let* ((window (window-for-id win))
	 (buff (buffer-text (window-buffer
			     window
			    "square")))
	(main-buff (window-buffer-main window)))
    (buffer-insert-undoable main-buff
			    #f
			    (gb->string buff))
    (buffer-undo-mark main-buff)
    (window-undo-stack-pop window)
    ;; Clean up and redraw
    (remove-window-buffer window 
			  "square")
    (buffer-invalidate (window-buffer-main window))
    (window-redraw win)))

;; Install

(define (square-tool-install)
  (add-mouse-move-hook square-mouse-move)
  (add-mouse-down-hook square-mouse-down)
  (add-mouse-up-hook square-mouse-up))

;; Uninstall

(define (square-tool-uninstall)
  (remove-mouse-move-hook square-mouse-move)
  (remove-mouse-down-hook square-mouse-down)
  (remove-mouse-up-hook square-mouse-up))

;; Register

(install-tool square-tool-install 
	      square-tool-uninstall
	      "Square"
	      "t" "s")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rectangle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define rectangle-tool-mouse-down #f)
(define rectangle-tool-mouse-down-x #f)
(define rectangle-tool-mouse-down-y #f)
  
;; When the button is pressed, make and initialise the drawing buffer

(define (rectangle-mouse-down win button x y) 
    (let* ((window (window-for-id win))
	   (buff (buffer-text (make-window-buffer window
						  "rectangle"))))
    (window-undo-stack-push window
			    buff)
    (set! rectangle-tool-mouse-down #t)
    (set! rectangle-tool-mouse-down-x (window-view-x window
						  x))
    (set! rectangle-tool-mouse-down-y (window-view-y window
						  y))))

(define (rectangle-mouse-move win raw-x raw-y) 
    (if rectangle-tool-mouse-down
	(let* ((window (window-for-id win))
	       (buff (window-buffer window
				    "rectangle"))
	       (x (window-view-x window 
				 raw-x))
	       (y (window-view-y window
				 raw-y)))
	  (gb-erase! (buffer-text buff))
	  (buffer-insert-undoable buff 
				  #f
				  "(path-begin)\n")
	  (buffer-insert-undoable buff
				  #f
				  (format #f 
					  "(move-to ~f ~f)~%"
					  rectangle-tool-mouse-down-x
					  rectangle-tool-mouse-down-y))
	  (buffer-insert-undoable buff
				  #f
				  (format #f 
					  "(line-to ~f ~f)~%"
					  x
					  rectangle-tool-mouse-down-y))
	  (buffer-insert-undoable buff
				  #f
				  (format #f 
					  "(line-to ~f ~f)~%"
					  x
					  y))
	  (buffer-insert-undoable buff
				  #f
				  (format #f 
					  "(line-to ~f ~f)~%"
					  rectangle-tool-mouse-down-x
					  y))
	  (buffer-insert-undoable buff 
				  #f
				  (format #f 
					  "(line-to ~f ~f)~%"
					  rectangle-tool-mouse-down-x
					  rectangle-tool-mouse-down-y))
	  (buffer-insert-undoable buff
				  #f
				  "(path-end)\n")
	  (buffer-undo-mark buff)
	  (buffer-invalidate buff)   
	  (window-redraw win))))

;; When the mouse is released, add the circle  buffer to the main buffer
;; Redraw the main buffer, and release the pen drawing buffers

(define (rectangle-mouse-up win button x y) 
  (set! rectangle-tool-mouse-down #f)
  (let* ((window (window-for-id win))
	 (buff (buffer-text (window-buffer
			     window
			    "rectangle")))
	(main-buff (window-buffer-main window)))
    (buffer-insert-undoable main-buff
			    #f
			    (gb->string buff))
    (buffer-undo-mark main-buff)
    (window-undo-stack-pop window)
    ;; Clean up and redraw
    (remove-window-buffer window 
			  "rectangle")
    (buffer-invalidate (window-buffer-main window))
    (window-redraw win)))

;; Install

(define (rectangle-tool-install)
  (add-mouse-move-hook rectangle-mouse-move)
  (add-mouse-down-hook rectangle-mouse-down)
  (add-mouse-up-hook rectangle-mouse-up))

;; Uninstall

(define (rectangle-tool-uninstall)
  (remove-mouse-move-hook rectangle-mouse-move)
  (remove-mouse-down-hook rectangle-mouse-down)
  (remove-mouse-up-hook rectangle-mouse-up))

;; Register

(install-tool rectangle-tool-install 
	      rectangle-tool-uninstall
	      "Rectangle"
	      "t" "r")

;; Star