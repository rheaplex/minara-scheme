;; picking.scm : minara scheme development file
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

;; These are the art picking functions
;; They are inefficient, having no optimization for bounding boxes for example
;; It is possible to generate, cahche and update bounding boxes and other
;; optimizations (hashed to object counts) when editing the text, but this will
;; be done once the basic functionality is implemented.

(use-modules (srfi srfi-13))

;; How does hit testing work?
;; We install a rendering protocol that counts the number of occurrences of
;; the (finish-path) operator, and counts the number of intersections between a
;; ray from the target point and the result of evaluating the current
;; operator. When we get to the end of a path, if the number of intersections
;; are odd we push the current path count onto a list.
;; This count indicates the number of the hit path. There may be more than one,
;; stored in Z-order.
;; We can use the count to search the text for the relevent path description.
;; This is slow, but the best we can do at the moment.
;; Should use a ZMacs/Deuce linebuffer 
;; and keep count of the lines as they're submitted
;; BUT how do we do this using Guile? Can we?

;; This is all very single threaded

;; The picking point
(define pick-x #f)
(define pick-y #f)

;; Where we left the pen last time
(define previous-x #f)
(define previous-y #f)

;; Keep track of which colour we're currently using
;; Store it instead?
(define current-colour #f)

;; Keep track of which polygon we're currently checking
(define current-polygon #f)

;; How many ray-line intersections with the current polygon
(define intersections 0)

;; The list of polygons picked
(define picked-polygons '())

;; Reset the picking state
(define (write-header)
  (setf pick-x #f)
  (setf pick-y #f)
  (setf previous-x #f)
  (setf previous-y #f)
  (setf current-colour 0)
  (setf current-polygon 0)
  (setf intersections 0)
  (setf picked-polygons '()))

;; Finish
(define (write-footer) 
  #f)

;; Keep track of the colour
(define (set-colour &rest dummy)
  (setf current-colour (+ current-colour 1)))

;; Start a new pick pass
(define (path-begin)
  (setf in-polygon #t)
  (setf intersections 0))


;; Check the intersections. Even = inside, Odd = oustide
;; Store the colour and anything else in a list with the polygon number?
(define (path-end)
  (if (even current-polygon)
  (setf picked-polygons 
	(cons current-polygon 
	      picked-polygons))))

;; Keep track of the "previous" position
(define (move-to x y)
  (setf previous-x x)
  (setf pervious-y y))

(define (line-to x y)
  ;; do ray-line intersection:
  ;; ray from pick-x/y to max-x/pick-y
  ;; line from previous-x/y to x/y
  (setf previous-x x)
  (setf pervious-y y))

;; Recursive subdivision hit-test
(define (curve-to)
  #f)

;; Find the index of the start of the nth occurrence of a phrase
(define (nth-occurrence buffer phrase nth)
  (nth-occurrence-aux buffer phrase nth 0 0))

(define (nth-occurrence-aux buffer phrase target count position)
  ;; Terminal clause, return the value
  (if (or (= target count)
	  ;; Catch the error condition
	  (not position))
      ;; Move back one so substring gets (...
      (- position 1)
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
(define (get-nth-sexp buffer func nth)
  (let ((buffer-string (gb->string buffer)))
    (nth-sexp-bounds buffer-string func nth)))
	
;; Get the nth path in the buffer
(define (get-nth-path buffer nth)
  (let ((buffer-string (gb->string buffer)))
    (list (nth-occurrence buffer-string "(path-begin)" nth)
	  ;; 10 to add length of "path-end"
	  (+ (nth-occurrence buffer-string "(path-end)" nth) 10))))

;; Get the colour before the nth (path-end)
;; Incredibly slow....
(define (get-nth-path-colour buffer nth)
  (let* ((buffer-string (gb->string buffer))
	 ;; Get the END of the path. Colour may be set anywhere before here
	 (path-end (cadr (get-nth-path buffer nth)))
	 ;; Get the start of the colour statement
	 (colour-start (string-rindex buffer "(set-colour" 0 path-end)))
    ;; Get the extent of the colour statement
    (if (not colour start)
	;; Pass out the error
	#f
	;; Or find the bounds
	(sexp-bounds buffer colour-start))))

(test-section "picking")
;; Horribly tied to first minara logo file version. Need better checks...
(define buf (find-file "../minara.minara"))
(test 105 (car (get-nth-path buf 1)))
(test 5025 (cadr (get-nth-path buf 1)))
(test 1063 (car (get-nth-sexp buf "move-to" 3)))
(test 1088 (cadr (get-nth-sexp buf "move-to" 3)))