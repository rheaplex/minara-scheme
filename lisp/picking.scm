;; picking.scm : object picking for minara
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

(define-module (minara picking)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-11)
  :use-module (srfi srfi-13)
  :use-module (minara transformations)
  :use-module (minara buffer)
  :use-module (minara window)
  :export (get-picked-path
	   pick-paths
	   pick-paths-window
	   pick-path
	   pick-path-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Picking in the main window buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pick-paths buf x y)
  (let ((buffer-string (buffer-to-string buf))
	(picking-module (resolve-module '(minara picking-protocol))))
    ((@ (minara picking-protocol) initialise-protocol) x y buffer-string) ;; Translate?
    (eval-string buffer-string picking-module)
    (let ((picked-polygons ((@ (minara picking-protocol) finalise-protocol))))
      (if (eq? picked-polygons '())
	  #f
	  picked-polygons))))

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