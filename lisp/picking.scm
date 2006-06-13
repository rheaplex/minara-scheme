;; picking.scm : minara scheme development file
;;
;; Copyright (c) 2004-2006 Rob Myers, rob@robmyers.org
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
;; Picking (object selection, highlighting, choosing, whatever)
;;
;; How does this work?
;;
;; Precis:
;; Count the shapes, hit-test the shapes, store the buffer positions that hit, 
;; find the s-expressions that contain those buffer positions.
;;
;; Details:
;; We install a rendering protocol that counts the number of 
;; occurrences of the (begin-path) operator. 
;; This allows us to identify which shape is being drawn.
;;
;; We then count the number of intersections between a ray
;; from the target point and the result of evaluating each drawing command.
;; When we get to the end of a path, if the number of intersections
;; are odd the point is inside the shape so we push the current path number 
;; onto a list.
;; This count indicates the number of the hit path. There may be more than one,
;; stored in Z-order.
;;
;; We then use the count to search the text for the relevent path description.
;;
;; This is slow, but we can cache a lot of the information and improve 
;; performance.
;;
;; Note that picking returns a list of every item under the picking point
;; from back to front rather than just the frontmost object. 
;; A normal "selection" tool can then disard everything apart from the topmost
;; object.
;;
;; Area-based selection will also be required and can be implemented similarly.
;; A point and a rectangle (or other shape eg pen-drawing based selection) are
;; just geometries to check for intersection or containment after all.
;;
;; This is all very single threaded. Attach the vars to the buffer being picked.
;;
;; And it's inefficient, having no optimization for bounding boxes for example
;; It is possible to generate, cache and update bounding boxes and other
;; optimizations (hashed to object counts) when editing the text, but this will
;; be done once the basic functionality is implemented.
;; Ideally we'd evaluate the buffer front-to-back. :-)
;; Nothing should be done or assumed to prevent the model of rebinding the 
;; drawing routines to the picking routines then evaluating the drawing buffer
;; from working.
;;
;; For picking inside of functions e.g. (define (square) moveto...) ... (square)
;; Rebind define to keep track of the call stack, or can we get the stack from
;; Guile?
;; Picking inside of functions is a TODO.
;;
;; Note that we will not be able to pick every imaginable piece of code, eg
;; random scribbles that do not have their seed in the main buffer won't pick,
;; and code from over the network may be problematic.
;; So provide guidelines for producing pickable code.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (srfi srfi-11))
(use-modules (srfi srfi-13))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A picking hit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type picking-hit
    (make-picking-hit index
		      from
		      to
		      transformation)
  picking-hit?
  ;; Which hit was it in the pick?
  (index picking-hit-index)
  ;; Which character in the buffer does the hit start at?
  (from picking-hit-from)
  ;; Which character in the buffer does the hit go to?
  (to picking-hit-to)
  ;; The transformation of the hit
  (transformation picking-hit-transform))
		      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals
;; Used within a single pass through the picking routines
;; (so should be thread-local)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The buffer string chopped to the end of the last matched sexp
;; RENAME
(define previous-buffer-string "")

;; The picking point
(define pick-x #f)
(define pick-y #f)

;; Where the last drawing operation left the pen
(define previous-x #f)
(define previous-y #f)

;; Keep track of which colour we're currently using
(define current-colour #f)

;; Keep track of the current transformations
(define current-translate #f)
(define current-rotate #f)
(define current-scale #f)
(define transformation-stack (make-matrix-stack))
(define current-transform #f)

;; RENAME

;; Keep track of which polygon we're currently drawing
(define current-polygon 0)
;; Keep track of the last polygon so we can skip polys to speed up sexp matching
(define previous-polygon 0)

;; How many ray-line intersections there are with the current polygon
(define intersections 0)

;; RENAME

;; The list of polygons picked and their transforms. This will be back-to-front.
(define picked-polygons '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Picking "render" protocol
;; Hang on, I thought we were meant to evaluate in a different environment,
;; not rebind?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-picking-rendering-protocol)
  (reset-picking-rendering-protocol)
  (set! set-colour picking-set-colour)
  (set! path-begin picking-path-begin)
  (set! path-end picking-path-end)
  (set! move-to picking-move-to)
  (set! line-to picking-line-to)
  (set! curve-to picking-curve-to)
  (set! push-matrix picking-push-matrix)
  (set! pop-matrix picking-pop-matrix)
  (set! concatenate-matrix picking-concatenate-matrix)
  (set! set-matrix picking-set-matrix)
  (set! identity-matrix picking-identity-matrix)
  (set! translate picking-translate)
  (set! rotate picking-rotate)
  (set! scale picking-scale))

;; Reset the picking state
(define (reset-picking-rendering-protocol)
  (set! pick-x #f)
  (set! pick-y #f)
  (set! previous-x #f)
  (set! previous-y #f)
  (set! current-colour 0)
  (set! current-polygon 0)
  (set! previous-polygon 0)
  (set! intersections 0)
  (set! current-rotate 0)
  (set! current-scale 0)
  (set! current-translate 0)
  (set! transformation-stack (make-matrix-stack))
  (set! picked-polygons '()))

;; Keep track of the colour
(define (picking-set-colour r g b a)
  (set! current-colour (+ current-colour 1)))

;; Keep track of the transforms
;; TODO: do it.

(define (picking-push-matrix)
    (set! transformation-stack 
	  (stack-push-matrix transformation-stack)))

(define (picking-pop-matrix)
    (set! transformation-stack 
	  (stack-pop-matrix transformation-stack)))

(define (picking-concatenate-matrix a b c d e f)
    (set! transformation-stack 
	  (stack-concatenate-matrix transformation-stack
				    (list a b c d e f))))

(define (picking-set-matrix a b c d e f)
    (set! transformation-stack 
	  (stack-set-matrix transformation-stack 
			    (list a b c d e f))))

(define (picking-identity-matrix)
    (set! transformation-stack 
	  (stack-set-matrix transformation-stack 
			    (identity-matrix))))

(define (picking-translate x y)
    (set! transformation-stack 
	  (stack-concatenate-matrix transformation-stack 
				    (matrix-translate-make x y)))
  (set! current-translate (+ current-translate 1)))

(define (picking-scale x y)
    (set! transformation-stack 
	  (stack-concatenate-matrix transformation-stack 
				    (matrix-scale-make x y)))
  (set! current-scale (+ current-scale 1)))

(define (picking-rotate theta)
    (set! transformation-stack 
	  (stack-concatenate-matrix transformation-stack 
				    (matrix-rotate-make theta)))
  (set! current-rotate (+ current-rotate 1)))

(define (picking-transform x y)
    (matrix-point-transform x y (stack-current-matrix transformation-stack)))

;; Start a new pick pass
(define (picking-path-begin)
  (set! intersections 0)
  (set! current-polygon (+ current-polygon 1)))

;; Check the intersections. Even = inside, Odd = oustide
;; Store the colour and anything else in a list with the polygon number?
(define (picking-path-end)
  (if (and (odd? intersections)
           (not (= intersections
                   0)))
     (set! picked-polygons 
	   (cons 
	    (get-picked-path)
	    picked-polygons)))
  (set! intersections 0))

;; Keep track of the "previous" position
(define (picking-move-to xx yy)
    (let-values (((x y) (picking-transform xx yy)))
		(set! previous-x x)
		(set! previous-y y)))

;; Where to send the ray -uh- line to. Oh, the horror! Fixme.

(define %ray-x 65535.0)
  
;; Line segment hit test

(define (picking-line-to xx yy)
    (let-values (((x y) (picking-transform xx yy)))
		(if (lines-intersect-vertices previous-x 
					      previous-y 
					      x 
					      y 
					      pick-x 
					      pick-y 
					      %ray-x 
					      pick-y)
		    (set! intersections (+ intersections
					   1)))
		(set! previous-x x)
		(set! previous-y y)))

;; Curve hit test

(define (picking-curve-to xx1 yy1 xx2 yy2 xx3 yy3)
    (let-values (((x1 y1) (picking-transform xx1 yy1))
		 ((x2 y2) (picking-transform xx2 yy2))
		 ((x3 y3) (picking-transform xx3 yy3)))
		(let ((count (line-bezier-intersection-count-vertices 
			      pick-x pick-y %ray-x pick-y
			      previous-x previous-y x1 y1 x2 y2 x3 y3)))
		  (set! previous-x x3)
		  (set! previous-y y3)
		  (set! intersections (+ intersections
					 count)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer routines
;; Find the positions in the buffer that match the s-expression that was
;; evaluated to draw a particular shape.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Find the index of the start of the nth occurrence of a phrase
(define (nth-occurrence buffer phrase nth)
  (nth-occurrence-aux buffer phrase nth 0 0))

(define (nth-occurrence-aux buffer phrase target count position)
  ;; Terminal clause, return the value
  (if (or (= target count)
	  ;; Catch the error condition
	  (not position))
      position
      ;; Otherwise search forward
      (nth-occurrence-aux buffer phrase target (+ count 1)
			  ;; +1 so we don't re-match the same string
			  (+ (string-contains buffer phrase position) 1))))

;; Get an s-expression from the ( at the character index given to the closing )
(define (sexp-bounds buffer start)
  (let ((end (sexp-bounds-aux buffer (+ start 1) 1)))
    (list start end)))

;; Recursively find the end of the s-expression
(define (sexp-bounds-aux buffer current count)
  ;; Terminal clause, return the value
  (if (= count 0)
      current
      ;; Otherwise we get the current char and check it
      (let ((current-char (substring buffer current (+ current 1))))
	(cond
	  ((string= current-char "(")
	   (sexp-bounds-aux buffer (+ current 1) (+ count 1)))
	  ((string= current-char ")") 
	   (sexp-bounds-aux buffer (+ current 1) (- count 1)))
	  (else
	   (sexp-bounds-aux buffer (+ current 1) count))))))
		  
;; Get the nth sexp starting with the given operator
(define (nth-sexp-bounds buffer operator count)	  
  (let* ((op-with-bracket (string-append "(" operator))
	 (start (nth-occurrence buffer op-with-bracket count)))
    (sexp-bounds buffer start)))

;; Get the nth colour statement in the buffer
(define (get-nth-sexp buffer-str func nth)
    (nth-sexp-bounds buffer-str func nth))
	
;; Get the nth path in the buffer
(define (get-nth-path buffer nth)
    (let ((path-from (nth-occurrence buffer
				      "(path-begin)" nth))
	   ;; 10 to move past "path-end"
	   (path-to (+ (nth-occurrence buffer 
				       "(path-end)" nth) 10)))
      (values path-from path-to)))

(define (get-picked-path)
    (let* ((nnth (- current-polygon previous-polygon)))
      (let-values (((path-from path-to)
		    (get-nth-path previous-buffer-string nnth)))
		  (set! previous-buffer-string (substring previous-buffer-string
							  path-to))
		  (set! previous-polygon current-polygon)
		  (make-picking-hit current-polygon
				    path-from
				    path-to
				    (copy-tree (stack-current-matrix
						transformation-stack))))))
  
;; Get the colour before the nth (path-end)
;; Incredibly slow....
;; (define (get-nth-path-colour buffer nth)
;;   (let* ((buffer-string (gb->string buffer))
;; 	 ;; Get the END of the path. Colour may be set anywhere before here
;; 	 (path-end (cadr (get-nth-path buffer nth)))
;; 	 ;; Get the start of the colour statement
;; 	 (colour-start (string-rindex buffer "(set-colour" 0 path-end)))
;;     ;; Get the extent of the colour statement
;;     (if (not colour start)
;; 	;; Pass out the error
;; 	#f
;; 	;; Or find the bounds
;; 	(sexp-bounds buffer colour-start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Picking in the main window buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-picking-point x y)
    ;; Translate?
    (set! pick-x x)
  (set! pick-y y))

(define (pick-paths buf x y)
    (install-picking-rendering-protocol)
  (set-picking-point x y) ;; Translate?
  (let ((buffer-string (buffer-to-string buf)))
    (set! previous-buffer-string buffer-string)
    (eval-string buffer-string))
  (if (eq? picked-polygons '())
      #f
      picked-polygons))

(define (pick-paths-window win x y)
    (pick-paths (window-buffer-main win) x y))

(define (pick-path buf x y)
    (let ((picks (pick-paths buf x y)))
      (if picks
	  ;; Return the range and transform
	  (last picks)
	  #f)))
  
(define (pick-path-window win x y)
    (pick-path (window-buffer-main win) x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;; Horribly tied to first minara logo file version. Need better checks...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(test-section "picking: s-expressions")
;;(define buf (find-file "../minara.minara"))
;;(test 105 (car (get-nth-path buf 1)))
;;(test 5025 (cadr (get-nth-path buf 1)))
;;(test 1063 (car (get-nth-sexp buf "move-to" 3)))
;;(test 1088 (cadr (get-nth-sexp buf "move-to" 3)))

;;(test-section "picking: picking")
;;(define %pickbuf (make-gap-buffer))
;;(gb-insert-string! %pickbuf
	;;		     ";;minara file\n(set-colour 0.0 0.0 1.0)\n(path-begin)\n(move-to 10 10)\n(line-to 10 100)\n(line-to 100 10)\n(line-to 10 10)\n(path-end)\n(fill-path)\n")
;;(test 40 (begin
;;	   (install-picking-rendering-protocol)
;;	   (eval-string (gb->string %pickbuf))))