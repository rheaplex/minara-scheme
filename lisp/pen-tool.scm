;; pen-tool.scm : the pen tool
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

(define pen-tool-mouse-down #f)
  
(define (pen-mouse-down win button x y) 
  (let ((close-buffer (add-overlay (window-for-id win)
					  "_pen_close"))
	(main-buffer (add-overlay (window-for-id win)
				  "_pen")))
    (gb-goto-char main-buffer (gb-point-max main-buffer))
    (gb-insert-string! main-buffer 
		       (format #f 
			       "(set-colour 0.0 0.828 0.387 1.0)~%(path-begin)~%(move-to ~a ~a)~%" 
			       x y))
    (gb-insert-string! close-buffer "(path-end)\n")
    (set! pen-tool-mouse-down #t)
    (multi-buffer-invalidate (window-overlays (window-for-id win)))))

(define (pen-mouse-move win x y) 
  (if pen-tool-mouse-down
      (let* ((the-buffer (window-overlay (window-for-id win) 
					    "_pen"))
	     (move (format #f "(line-to ~a ~a)~%" x y)))
	(gb-goto-char the-buffer (gb-point-max the-buffer))
	(gb-insert-string! the-buffer move)
	(multi-buffer-invalidate (window-overlays (window-for-id win)))  
	(window-invalidate win))))

(define (pen-mouse-up win button x y) 
  (set! pen-tool-mouse-down #f)
  (let* ((buff (window-overlay 
		(window-for-id win)
		"_pen"))
	 (close-buffer (window-overlay 
			(window-for-id win)
			"_pen_close"))
	 (main-buff (window-buffer-for-id win)))
    ;; Add the overlay to the main buffer
    (gb-goto-char main-buff (gb-point-max main-buff))
    (gb-insert-string! main-buff (gb->string buff))
    (gb-goto-char main-buff (gb-point-max main-buff))
    (gb-insert-string! main-buff (gb->string close-buffer))
    ;; Clean up and redraw
    (remove-overlay (window-for-id win) "_pen")
    (remove-overlay (window-for-id win) "_pen_close"))
  (cached-buffer-invalidate (window-cached-buffer-for-id win))
  (window-invalidate win))


;; Install

(define (pen-tool-install)
  (add-mouse-move-hook pen-mouse-move)
  (add-mouse-down-hook pen-mouse-down)
  (add-mouse-up-hook pen-mouse-up))

;; Uninstall

(define (pen-tool-uninstall)
  (remove-mouse-move-hook pen-mouse-move)
  (remove-mouse-down-hook pen-mouse-down)
  (remove-mouse-up-hook pen-mouse-up))

;; Register

(install-tool pen-tool-install 
	      pen-tool-uninstall
	      "Pen"
	      "t" "p")