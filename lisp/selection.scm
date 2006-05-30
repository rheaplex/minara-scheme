;; selection.scm : minara scheme development file
;;
;; Copyright (c) 2006 Rob Myers, rob@robmyers.org
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

;; This is the selection list on the buffer
;; Selection mathematics in picking.scm
;; Copy & paste operations in copy-and-paste.scm

;; !!! Make sure these work with undo!!!
;; Well make adding/removing to the list undo actions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (srfi srfi-13))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selection list handling
;; A list of dotted ranges eg '() , '((12. 47)) or '((12 .47) (123 456))
;; The selection tool replaces or appends depending on a modifier key (shift?).
;; Undoable
;; Consumed by a copy/paste delete, restored if they undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: removing a range that matches an existing value, 
;;       for selective deselecting.

(define (selection-ranges-var buffer)
    (buffer-variable buffer "_selections"))

(define (clear-selection-ranges-var buffer)
    (buffer-variable-set-undoable buffer 
				  "_selections"
				  '()))

(define (sort-selection-ranges ranges)
    (sort ranges
	  (lambda (a b)
	    (< (car a)
	       (car b)))))

(define (append-selection-range ranges from to)
    (cons (cons from to) ranges))

;; Combine any overlapping ranges into a single range
;; Assumes they're in order
;; So get first one, get next, if overlap discard first & combine,
;; otherwise 

(define (selections-overlap? a b)
    (and (> (cdr a)
	    (car b))
	 (< (car a)
	    (cdr b))))

(define (combine-ranges a b)
    (cons (min (car a) (car b))
	  (max (car a) (cdr b))))

(define (combine-selection-ranges-aux range ranges sorted)
    (if (ranges)
	(let ((next-range (car ranges))
	      (rest (cdr ranges)))
	  (if (selections-overlap? range next-range)
	      (combine-selection-ranges-aux (car rest)
					    (cdr rest)
					    (cons (combine-ranges range
								  next-range)
						  sorted)))
	  (combine-selection-ranges-aux next-range
					rest
					(cons range sorted))))
  (cons range sorted))
  
(define (combine-selection-ranges ranges)
    (combine-selection-ranges-aux (car ranges) (cdr ranges)))

(define (append-selection-ranges-var buffer range)
    (buffer-variable-set-undoable buffer
				  "_selections"
				  (sort-selection-renages
				   (combine-selection-ranges
				    (append-selection-range 
				     (selection-ranges-var buffer)
				     range)))))

(define (set-selection-ranges-var buffer range)
    (buffer-variable-set-undoable buffer
				  "_selections"
				  (list range)))

;; Assumes ranges are sorted highest to lowest and do not overlap

(define (delete-selection-ranges buffer)
    (for-each 
     (lambda (range)
       (buffer-delete-undoable buffer (car range) (cdr range)))
     (selection-ranges-var buffer))
  (clear-selection-ranges-var buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy buffer (clipboard) handling
;; This is a buffer, but it is not on the buffer stack, 
;; it's in a buffer variable on the main buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Write a general (ensure-buffer-variable buff name value) MACRO

(define (ensure-copy-buffer-var buffer)
    (or (buffer-variable buffer 
			 "_copy")
        (progn (set-buffer-variable! buffer
				     "_copy"
				     (make-buffer))
	       (buffer-variable buffer 
				"_copy"))))
	
(define (copy-selection-ranges-to-buffer main-buffer to-buffer)
    (for-each
     (lambda (range)
       (buffer-insert-undoable to-buffer
			       #f
			       (buffer-range-to-string main-buffer 
						       (car range) 
						       (cdr range))))
     (selection-ranges-var main-buffer)))

(define (copy-selection-ranges-to-copy-buffer-var buffer)
    (let ((copy-buf (ensure-copy-buffer-var buffer)))
      (copy-selection-ranges-to-buffer main-buffer to-buffer)))
  
(define (cut-selection-ranges-to-copy-buffer-var buffer)
    (copy-selection-ranges-to-copy-buffer-var buffer)
  (clear-selection-ranges-var buffer))

(define (paste-copy-buffer-var buffer)
    (buffer-insert-undoable buffer
			    #f
			    (buffer-to-string (ensure-copy-buffer-var buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-copy-key)
    (copy-selection-ranges-to-copy-buffer-var (window-buffer (window-current))))

(keymap-add-fun %global-keymap 
		do-copy-key
		"Cc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-delete-key)
    (delete-selection-ranges (window-buffer (window-current))))

(keymap-add-fun %global-keymap 
		do-delete-key
		"Cd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cut
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-cut-key)
    (cut-selection-ranges-to-copy-buffer-var (window-buffer (window-current))))

(keymap-add-fun %global-keymap 
		do-cut-key
		"Cx")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-paste-key)
    (clear=selection-ranges-var (window-buffer (window-current))))

(keymap-add-fun %global-keymap 
		do-paste-key
		"Cv")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clear Selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-clear-selection-key)
    (clear-copy-buffer-var (window-buffer (window-current))))

(keymap-add-fun %global-keymap 
		do-clear-selection-key
		"s")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (select-mouse-up win button x y)
    (let ((selection (pick-path-window win x y)))
      (if selection
	  (begin
	   (if (= button 1)
	       (set-selection-ranges-var (window-buffer-main win)
					 selection)
	       (append-selection-ranges-var (window-buffer-main win)
					    selection))
	   (highlight-selection win))
	  (clear-highlight-selection win))))

;; Install

(define (select-tool-install)
  (add-mouse-up-hook select-mouse-up))

;; Uninstall

(define (select-tool-uninstall)
  (remove-mouse-up-hook select-mouse-up))

;; Register

(install-tool select-tool-install 
	      select-tool-uninstall
	      "Select"
	      "s" "s")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selection highlighting
;; Copy the selections to a buffer, with translation code prepended.
;; The translation code gets the translation variables from the buffer
;;  when evaluating.
;; We also override the colour setting and temporarily rebind set-colour.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (highlight-selection win)
    (let ((highlight-buffer (ensure-window-buffer win "_highlight")))
      (set-buffer-variable! highlight-buffer "x" 0.0)
      (set-buffer-variable! highlight-buffer "y" 0.0)
      (buffer-insert-undoable 
       highlight-buffer
       #f
       "(translate (buffer-variable (current-buffer) \"x\") (buffer-variable (current-buffer) \"x\"))\n(set-colour 1.0 0.0 0.0 0.0)\n(define old-set-colour set-colour)\n(set! set-colour (lambda (a b c d) #f))\n")
      (copy-selection-ranges-to-buffer (window-buffer-main win) 
				       highlight-buffer)
      (buffer-insert-undoable 
       highlight-buffer
       #f
       "\n(set! set-colour old-set-colour)\n")
      (buffer-invalidate highlight-buffer))
  (window-redraw win))

(define (clear-highlight-selection win)
    (remove-window-buffer win "_highlight"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move tool
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define move-tool-mouse-down #f)
(define move-tool-mousedown-x #f)
(define move-tool-mousedown-y #f)

(define (move-mouse-down win button x y) 
    (set! move-tool-mouse-down #t)
    (set! move-tool-mousedown-x 
	  x)
    (set! move-tool-mousedown-y 
	  y))

(define (move-mouse-move win x y) 
  (if move-tool-mouse-down
      (let ((highlight-buffer (window-buffer window "_highlight")))
	(set-buffer-variable highlight-buffer
			     "x"
			     (- x;;(window-view-x window x)
				move-tool-mousedown-x))
	(set-buffer-variable! move-buffer
			      "y"
			      (- y;;(window-view-y window y)
				 move-tool-mousedown-y))
	(window-view-update window))))

(define (translate-selections win)
    ;; To translate the original...
    ;; Foreach range in the selection variable
    ;; See if the object has a translate devoted to it.
    ;; If so, modify that.
    ;; If not, add one.
    #f)

(define (move-mouse-up win button x y)
    (translate-selections win)
  (set! move-tool-mouse-down #f))

;; Install

(define (move-tool-install)
  (add-mouse-move-hook move-mouse-move)
  (add-mouse-down-hook move-mouse-down)
  (add-mouse-up-hook move-mouse-up))

;; Uninstall

(define (move-tool-uninstall)
  (remove-mouse-move-hook move-mouse-move)
  (remove-mouse-down-hook move-mouse-down)
  (remove-mouse-up-hook move-mouse-up))

;; Register

(install-tool move-tool-install 
	      move-tool-uninstall
	      "Move"
	      "t" "m")