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
;; Selection mathematics is in picking.scm

;; !!! Make sure these work with undo!!!
;; Well make adding/removing to the list undo actions


;; Can't combine ranges if transforms clash
;; Copy all transforms for each selection?
;; So get all before selection but discard on push/pop? (optimise later)
;; eg. collect-transforms-for in picking <----- !!!!!
;;
;; Or ignore smaller selections && replace with larger selections
;; So save transform in pick information for each hit:
;; (struct pick-hit index from to transform)




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

(define (selections-var buffer)
    (buffer-variable buffer "_selections"))

(define (clear-selections-var buffer)
    (buffer-variable-set-undoable buffer 
				  "_selections"
				  '()))

(define (sort-selections selections)
    (sort selections
	  (lambda (a b)
	    (< (picking-hit-from a)
	       (picking-hit-from b)))))

(define (append-selection selections selection)
    (cons selection selections))

;; Combine any overlapping selections into a single range
;; Assumes they're in order
;; So get first one, get next, if overlap discard first & combine,
;; otherwise 

;; REDO: Overlaps need reworking in light of the way transforms work

(define (selections-overlap? a b)
    (and (> (cdr a)
	    (car b))
	 (< (car a)
	    (cdr b))))

(define (combine-selections a b)
    (cons (min (car a) (car b))
	  (max (car a) (cdr b))))

(define (combine-selections-aux range selections sorted)
    (if (selections)
	(let ((next-range (car selections))
	      (rest (cdr selections)))
	  (if (selections-overlap? range next-range)
	      (combine-selections-aux (car rest)
					    (cdr rest)
					    (cons (combine-selections range
								      next-range)
						  sorted)))
	  (combine-selections-aux next-range
					rest
					(cons range sorted))))
  (cons range sorted))
  
(define (combine-selections selections)
    (combine-selections-aux (car selections) (cdr selections)))

(define (append-selections-var buffer range)
    (buffer-variable-set-undoable buffer
				  "_selections"
				  (sort-selection-ranges
				   (combine-selections
				    (append-selection-range 
				     (selections-var buffer)
				     range)))))

(define (set-selections-var buffer range)
    (buffer-variable-set-undoable buffer
				  "_selections"
				  (list range)))

;; Assumes selections are sorted highest to lowest and do not overlap

(define (delete-selections buffer)
    (for-each 
     (lambda (pick)
       (buffer-delete-undoable buffer 
			       (picking-hit-from pick) 
			       (picking-hit-to pick)))
     (selections-var buffer))
  (clear-selections-var buffer))

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

(define (copy-selections-to-buffer main-buffer to-buffer)
    (for-each
     (lambda (selection)
       (let ((text  
	      (format #f "(push-matrix)~%~a~%~a~%(pop-matrix)~%"
		      (matrix-to-concatenate-string 
		       (picking-hit-transform selection))
		      (buffer-range-to-string 
		       main-buffer 
		       (picking-hit-from selection) 
		       (picking-hit-to selection)))))
	 (buffer-insert-no-undo to-buffer
				 #f
				 text)))
     (selections-var main-buffer)))

(define (copy-selections-to-copy-buffer-var buffer)
    (let ((copy-buf (ensure-copy-buffer-var buffer)))
      (copy-selections-to-buffer main-buffer to-buffer)))
  
(define (cut-selections-to-copy-buffer-var buffer)
    (copy-selections-to-copy-buffer-var buffer)
  (clear-selections-var buffer))

(define (paste-copy-buffer-var buffer)
    (buffer-insert-undoable buffer
			    #f
			    (buffer-to-string (ensure-copy-buffer-var buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-copy-key)
    (copy-selections-to-copy-buffer-var (window-buffer (window-current))))

(keymap-add-fun %global-keymap 
		do-copy-key
		"Cc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-delete-key)
    (delete-selections (window-buffer (window-current))))

(keymap-add-fun %global-keymap 
		do-delete-key
		"Cd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cut
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-cut-key)
    (cut-selections-to-copy-buffer-var (window-buffer (window-current))))

(keymap-add-fun %global-keymap 
		do-cut-key
		"Cx")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do-paste-key)
    (clear=selections-var (window-buffer (window-current))))

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
	       (set-selections-var (window-buffer-main win)
					 selection)
	       (append-selections-var (window-buffer-main win)
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
      (buffer-undo-mark highlight-buffer)
      (buffer-insert-no-undo 
       highlight-buffer
       #f
       "(push-matrix)(translate (buffer-variable (current-buffer) \"x\") (buffer-variable (current-buffer) \"y\"))\n(set-colour 1.0 0.0 0.0 0.0)\n(set! set-colour (lambda (a b c d) #f))\n")
      (copy-selections-to-buffer (window-buffer-main win) 
				       highlight-buffer)
      (buffer-insert-no-undo 
       highlight-buffer
       #f
       "\n(set! set-colour rendering:set-colour)(pop-matrix)\n") ;; Restore col!
      (buffer-invalidate highlight-buffer)
      (window-redraw win)))

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
      (let ((highlight-buffer (window-buffer win "_highlight")))
	(set-buffer-variable! highlight-buffer
			     "x"
			     (- x ;;(window-view-x window x)
				     move-tool-mousedown-x))
	(set-buffer-variable! highlight-buffer
			      "y"
			      (- y ;;(window-view-y window y)
				      move-tool-mousedown-y))
	(format #t "~a ~a~%" (buffer-variable highlight-buffer "x")
		(buffer-variable highlight-buffer "y"))
	(buffer-invalidate highlight-buffer)
	(window-redraw win))))

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