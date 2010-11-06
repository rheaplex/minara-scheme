;; picking-protocol.scm : the picking buffer evaluation protocol for minara
;;
;; Copyright (c) 2004-2006 Rob Myers, rob@robmyers.org
;;
;; This file is part of minara.
;;
;; minara is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; minara is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


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

(define-module (minara picking-protocol)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-11)
  :use-module (srfi srfi-13)
  :use-module (minara transformations)
  :use-module (minara geometry)
  :use-module (minara sexp)
  :use-module (minara buffer)
  :use-module (minara picking-hit)
  :export (initialise-protocol
	   path-begin
	   path-end 
	   move-to
	   line-to 
	   curve-to 
	   set-colour
	   begin-mask 
	   end-mask 
	   begin-masking
	   end-masking 
	   push-matrix 
	   pop-matrix
	   concatenate-matrix 
	   set-matrix 
	   identity-matrix
	   translate 
	   scale 
	   rotate
	   finalise-protocol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals
;; Used within a single pass through the picking routines
;; (so should be thread-local)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The buffer string chopped to the end of the last matched sexp
;; RENAME
(define previous-buffer-string "")
		      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals
;; Used within a single pass through the picking routines
;; (so should be thread-local)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The picking point
(define pick-x #f)
(define pick-y #f)

;; Where the last drawing operation left the pen
(define previous-x #f)
(define previous-y #f)

;; Keep track of which colour we're currently using
;; (We'll need to keep track of any other fill methods as well,
;;  but we will never stroke, so we won't need to track that.)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reset the picking state
(define (initialise-protocol x y buffstr)
  (set! pick-x x)
  (set! pick-y y)
  (set! previous-buffer-string buffstr)
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

(define (finalise-protocol)
  picked-polygons)

;; Keep track of the colour
(define (set-colour r g b a)
  (set! current-colour (+ current-colour 1)))

;; Keep track of the transforms
;; TODO: do it.

(define (push-matrix)
    (set! transformation-stack 
	  (stack-push-matrix transformation-stack)))

(define (pop-matrix)
    (set! transformation-stack 
	  (stack-pop-matrix transformation-stack)))

(define (concatenate-matrix a b c d e f)
    (set! transformation-stack 
	  (stack-concatenate-matrix transformation-stack
				    (list a b c d e f))))

(define (set-matrix a b c d e f)
    (set! transformation-stack 
	  (stack-set-matrix transformation-stack 
			    (list a b c d e f))))

(define (identity-matrix)
    (set! transformation-stack 
	  (stack-set-matrix transformation-stack 
			    (identity-matrix))))

(define (translate x y)
    (set! transformation-stack 
	  (stack-concatenate-matrix transformation-stack 
				    (matrix-translate-make x y)))
  (set! current-translate (+ current-translate 1)))

(define (scale x y)
    (set! transformation-stack 
	  (stack-concatenate-matrix transformation-stack 
				    (matrix-scale-make x y)))
  (set! current-scale (+ current-scale 1)))

(define (rotate theta)
    (set! transformation-stack 
	  (stack-concatenate-matrix transformation-stack 
				    (matrix-rotate-make theta)))
  (set! current-rotate (+ current-rotate 1)))

(define (transform x y)
    (matrix-point-transform x y (stack-current-matrix transformation-stack)))

;; Start a new pick pass
(define (path-begin)
  (set! intersections 0)
  (set! current-polygon (+ current-polygon 1)))

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

;; Check the intersections. Even = inside, Odd = oustide
;; Store the colour and anything else in a list with the polygon number?
(define (path-end)
  (if (and (odd? intersections)
           (not (= intersections
                   0)))
     (set! picked-polygons 
	   (cons 
	    (get-picked-path)
	    picked-polygons)))
  (set! intersections 0))

;; Keep track of the "previous" position
(define (move-to xx yy)
    (let-values (((x y) (transform xx yy)))
		(set! previous-x x)
		(set! previous-y y)))

;; Where to send the ray -uh- line to. Oh, the horror! Fixme.

(define %ray-x 65535.0)
  
;; Line segment hit test

(define (line-to xx yy)
    (let-values (((x y) (transform xx yy)))
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

(define (curve-to xx1 yy1 xx2 yy2 xx3 yy3)
    (let-values (((x1 y1) (transform xx1 yy1))
		 ((x2 y2) (transform xx2 yy2))
		 ((x3 y3) (transform xx3 yy3)))
		(let ((count (line-bezier-intersection-count-vertices 
			      pick-x pick-y %ray-x pick-y
			      previous-x previous-y x1 y1 x2 y2 x3 y3)))
		  (set! previous-x x3)
		  (set! previous-y y3)
		  (set! intersections (+ intersections
					 count)))))
  