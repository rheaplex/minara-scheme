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

;; Ray-line intersection

;; Evaluate the bezier at time t
;; Converted from the C in minara_rendering.c

(define bezier-eval ·(h0 v0 h1 v1 h2 v2 h3 v3 t)
  (let ((q1 (+ (* t t t -1.0) (* t t 3) (* t -3.0) 1.0))
	(q2 (+ (* t t t 3.0) (* t t -6.0) (* t 3.0)))
	(q3 (+ (* t t t -3.0) (* t t 3.0)))
	(q4 (* t t t)))
    (let ((qx (+ (* q1 h0) (* q2 h1) (* q3 h2) (* q4 h3)))
	  (qy (+ (* q1 v0) (* q2 v1) (* q3 v2) (* q4 v3))))
      (list qx qy))))

;; Point mathematics

(define add-point (h0 v0 h1 v1)
  (list (+ h0 h1) (+ v0 v1)))

(define divide-point (h v d)
  (list (/ h d) (/ v d)))

(define point-x (p)
  (first p))

(define point-y (p)
  (second p))


;; Divide the bezier into two equal halves
;; Left first, then right. Two lists of vertex co-ordinates
(define split-bezier (h0 v0 h1 v1 h2 v2 h3 v3)
  (let* ((p01 (/ (add-point h0 v0 h1 v1) 2.0))
	 (p12 (/ (add-point h1 v1 h2 v2) 2.0))
	 (p23 (/ (add-point h2 v2 h3 v2) 2.0))·
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


;; Ray-bezier intersection

;; Get the normal of the bezier at point t