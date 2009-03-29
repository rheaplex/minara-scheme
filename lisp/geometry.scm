;; geometry.scm : minara scheme development file
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
;; Mathematical functions for geometric calculations.
;; Particularly for hit-testing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Find the distance between the points x1,y1 and x2,y2

(define (distance-between-points x1 y1 x2 y2)
  (sqrt (+ (expt (abs (- x2 
		    x1))
		 2.0)
	   (expt (abs (- y2
			 y1))
		 2.0))))

;; Find polar angle of point px,py around origin ox,oy

(define (angle-around-point ox oy px py)
    (let ((x (- px
		ox))
	  (y (- py
		oy)))
      (case x
	((0.0)
	 (cond 
	   ((< y 0.0) 
	    270.0)
	   ((> v 0.0)
	    90.0)
	   ((= v 0.0)
	    0.0)))
	(else
	 (let ((r (* (atan (/ y
			      x)) 
		     (/ 180.0
			3.14159))))
	   (if (< x 0.0)
	       (+ r
		  180.0)
	       r))))))

(define $pi 
    3.1415926535897932384626433832795029)

(define $degrees-to-radians
    (/ $pi
       180.0))

(define (degrees-to-radians degrees)
    (* degrees
       $degrees-to-radians))

(define (rotate-point-around-point x1 y1 x2 y2 theta)
    (let ((st (sin (degrees-to-radians theta)))
	  (ct (cos (degrees-to-radians theta)))
	  (x (- x2
		x1))
	  (y (- y2
		y1)))
      (cons (+ x1 
	       (- (* x
		     ct)
		  (* y
		   st)))
	    (+ y1 
	       (+ (* y
		     ct)
		  (* x
		     st))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line-line intersection
;; http://astronomy.swin.edu.au/~pbourke/geometry/lineline2d/
;; Returns the t where the second line intersects the first line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lines-intersect-vertices p1x p1y p2x p2y ;; First line 
				  p3x p3y p4x p4y);; Second line 
    (let ((denominator (- (* (- p4y p3y)
			     (- p2x p1x)) 
			  (* (- p4x p3x)
			     (- p2y p1y)))))
      (if (= denominator 0.0)
	  #f ;; Parallel lines
	  (let ((ua (/ (- (* (- p4x p3x) 
			     (- p1y p3y))
			  (* (- p4y p3y) 
			     (- p1x p3x)))
		       denominator))
		(ub (/ (- (* (- p2x p1x) 
			     (- p1y p3y)) 
			  (* (- p2y p1y) 
			     (- p1x p3x))) 
		       denominator)))
	    (if (and (>= ua 0.0)
		     (<= ua 1.0)
		     (>= ub 0.0)
		     (<= ub 1.0)) ;; Intersection (or not)
		ua
		#f)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find out which side of an infinite line through p1 and p2 that p0 lies on.
;;   < 0 = left, > 0 = right, == 0 = exactly on.
;; From draw-something :-)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (point-line-side p0 p2 p1)
    (- (* (- (x p1) (x p0)) (- (y p2) (y p0)))
       (* (- (x p2) (x p0)) (- (y p1) (y p0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Point mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-point h0 v0 h1 v1)
  (list (+ h0 h1) (+ v0 v1)))

(define (divide-point h v d)
  (list (/ h d) (/ v d)))

(define (point-x p)
  (first p))

(define (point-y p)
  (second p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beziers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evaluate the bezier at time t
;; Converted from the C in minara_rendering.c

(define (bezier-eval h0 v0 h1 v1 h2 v2 h3 v3 t)
  (let ((q1 (+ (* t t t -1.0) (* t t 3) (* t -3.0) 1.0))
	(q2 (+ (* t t t 3.0) (* t t -6.0) (* t 3.0)))
	(q3 (+ (* t t t -3.0) (* t t 3.0)))
	(q4 (* t t t)))
    (let ((qx (+ (* q1 h0) (* q2 h1) (* q3 h2) (* q4 h3)))
	  (qy (+ (* q1 v0) (* q2 v1) (* q3 v2) (* q4 v3))))
      (list qx qy))))

;; Divide the bezier into two equal halves
;; Left first, then right. Two lists of vertex co-ordinates

(define (split-bezier h0 v0 h1 v1 h2 v2 h3 v3)
  (let* ((p01 (/ (add-point h0 v0 h1 v1) 2.0))
	 (p12 (/ (add-point h1 v1 h2 v2) 2.0))
	 (p23 (/ (add-point h2 v2 h3 v2) 2.0))?
	 (p012 (/ (add-point (point-x p01) (point-y p01)
			     (point-x p12) (point-y p12)) 2.0))
	 (p123 (/ (add-point (point-x p12) (point-y p12)
			     (point-x p23) (point-y p23)) 2.0))
	 (p0123 (/ (add-point (point-x p012) (point-y p012)
			      (point-x p123) (point-y p123)) 2.0)))
    (list (list h0 v0 h01 v01 (point-x 012) (point-y 012) 
		(point-x 0123) (point-y 0123))
	  (list (point-x 0123) (point-y 0123) (point-x 123) (point-y 123)
		h23 v23 h3 v3))))

;; Decide the flatness of the bezier

;; Line-bezier intersection
;; Terrible. Almost as good as our bezier drawing
;; Replace with something less embarrasingly awful, 
;; recursive subdivision at least
;; And the name is bad, too

(define %bez-eval-steps 10)
(define %bez-eval-step (/ 1.0 
                          %bez-eval-steps))

(define (line-bezier-intersection-count-vertices 
	 ax ay bx by h0 v0 h1 v1 h2 v2 h3 v3)
  (let ((crossings '())
        (ph h0)
        (pv v0))
    ;; Step through the bezier at a very coarse resolution
    (do ((t %bez-eval-step (+ t %bez-eval-step)))
	;; Return the count of intersections
	((> t 1.0) (length crossings))
      (let* ((p (bezier-eval h0 v0 h1 v1 h2 v2 h3 v3 t))
	     (h (point-x p))
	     (v (point-y p))
	     (ti (lines-intersect-vertices ph pv h v
					   ax ay bx by)))
	;; Counting the number of intersections
	;; Ignoring intersections at 0.0 because 
	;; they are the same as the previous 1.0 intersection...
	(if (and ti
		 (> ti 
		    0.0))
	    (let ((intersection (cons h v)))
	      ;; Much siliness to avoid duplicate points from adjacent sections
	      ;; when the ray passes exactly through the point
	      (set! crossings (assoc-set! crossings 
					  intersection 
					  #t))))
	(set! ph h)
	(set! pv v)))))

  
;; Get the normal of the bezier at point t


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-section "geometry: intersection")
(test 0.0 (lines-intersect-vertices 0.0 0.0 0.0 100.0 
				    0.0 0.0 100.0 0.0))
(test 0.5 (lines-intersect-vertices 0.0 0.0 0.0 100.0 
				    0.0 50.0 100.0 50.0))
(test 1.0 (lines-intersect-vertices 0.0 0.0 0.0 100.0 
				    0.0 100.0 100.0 100.0))
(test 0.5 (lines-intersect-vertices 0.0 0.0 100.0 100.0 
				    0.0 50.0 100.0 50.0))
(test #f (lines-intersect-vertices 0.0 0.0 100.0 100.0 
				   1000.0 1000.0 1000.0 1000.0))
(test 0 (line-bezier-intersection-count-vertices 20 0 80 0
						 0 0 0 100 100 100 100 0))
;; Aligned with end-point of a subdivision (given t step of 0.1)
(test 1 (line-bezier-intersection-count-vertices 50 0 50 150
						 0 0 0 100 100 100 100 0))
;; Not aligned with end-point of subdivision (given t step of 0.1)
(test 1 (line-bezier-intersection-count-vertices 52 0 52 150
						 0 0 0 100 100 100 100 0))
(test 2 (line-bezier-intersection-count-vertices 0 50 100 50
						 0 0 0 100 100 100 100 0))
