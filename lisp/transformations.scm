;; transformations.scm : minara scheme development file
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

(use-modules (srfi srfi-1))

;; matrices are Postscript-style 6-element arrays

;; identity

(define (identity)
  (list 1.0 0.0 0.0 1.0 0.0 0.0))

;; scale

(define (scale x y)
  (list x 0.0 0.0 y 0.0 0.0))

;; translate

(define (translate x y)
  (list 1.0 0.0 0.0 1.0 x y))

;; rotate

(define (rotate z)
  (let ((c (cos z))
	(s (sin z))
	(ns (- (sin z))))
  (list c s nc c 0.0 0.0)))

;; concatenate
;; This is going to be a bottleneck, so unroll it
;; Optimise so rather than first, second, third, we use car/cdr on each cdr

(define (concatenate a b)
  (let* ((a1 (first a))
	 (a2 (second a))
	 (a3 (third a))
	 (a4 (fourth a))
	 (a5 (fifth a))
	 (a6 (sixth a))
	 (b1 (first b))
	 (b2 (second b))
	 (b3 (third b))
	 (b4 (fourth b))
	 (b5 (fifth b))
	 (b6 (sixth b))
	 (((+ (* a1 b1) 
	      (* a2 b2))
	   (+ (* (first a1) (second b1)) 
	      (* (second a1) (second b2)))
	   (+ (* (first a1) (third b1)) 
	      (* (second a1) (third b2)))
	  ((+ (* (first a2) (first b1)) 
	      (* (second a2) (first b2)))
	   (+ (* (first a2) (second b1)) 
	      (* (second a2) (second b2)))
	   (+ (* (first a2) (third b1)) 
	      (* (second a2) (third b2)))
	  ((+ (* (first a3) (first b1)) 
	      (* (second a3) (first b2)))
	   (+ (* (first a3) (second b1)) 
	      (* (second a3) (second b2)))
	   (+ (* (first a3) (third b1)) 
	      (* (second a3) (third b2))))))))))

;; concatenaten
;; Concatenate a list of matrices

(define (concatenaten a . ms)
  (let ((product (concatenate a (car ms)))
	(rest (cdr ms)))
    (if (nilp rest)
	product
	(concatenaten product (cdr ms)))))

;; transform
;; Transform a point by a matrix
;; This is going to be a bottleneck, so unroll it
;; Optimise so rather than first, second, third, we use car/cdr on each cdr

(define (transform x y m)
  (let* ((a (first m))
	 (b (second m))
	 (c (third m))
	 (d (fourth m))
	 (tx (fifth m))
	 (ty (sixth m))
	 (xx (+ (* x a) 
		(* y b)
		tx))
	 (yy (+ (* x c)
		(* y d)
		ty)))
    (cons xx yy))) ;; Dotted list

;; %ctm
;; Current transformation matrix, thread-local

(define %ctm (identity))

;; %ctms
;; Current transformation matrices stack, thread-local

(define %ctms '())

;; push-ctm

(define (push-ctm m)
  (push %ctm %ctms)
  (concatenate m %ctm))

;; pop-ctm

(define (pop-ctm)
  (if (not (nilp (cdr %ctms)))
      (set! %ctm (car %ctms))
      (set! %ctms (cdr %ctms))))

;; get-ctm

(define (get-ctm)
  %ctm)