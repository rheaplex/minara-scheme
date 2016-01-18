;; transformations.scm : geometric transformations for minara
;;
;; Copyright (c) 2004 Rob Myers, rob@robmyers.org
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
;; Geometric transformations using matrices
;; Matrices are Postscript-style 6-element arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (minara transformations)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-14)
  :export (matrix-identity-make
           matrix-scale-make
           matrix-translate-make
           matrix-rotate-make
           matrix-to-concatenate-string
           matrix-to-string
           matrix-concatenate
           matrix-concatanaten
           matrix-point-transform
           make-matrix-stack
           stack-set-matrix
           stack-concatenate-matrix
           stack-current-matrix
           stack-push-matrix
           stack-pop-matrix
           get-translate-values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to generate transformation matrices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a  b  0
;; c  d  0
;; tx ty 1

;; (a b c d tx ty)

;; identity

;; 1  0  0
;; 0  1  0
;; 0  0  1

;; (1 0 0 0 1 0)

(define (matrix-identity-make)
  (list 1.0 0.0 0.0 1.0 0.0 0.0))

;; scale

;; sx 0  0
;; 0  sy 0
;; 0  0  1

;; (sx 0 0 sy 0 0)

(define (matrix-scale-make x y)
  (list x 0.0 0.0 y 0.0 0.0))

;; translate

;; 1  0  0
;; 0  1  0
;; tx ty 1

;; (1 0 0 1 tx ty)

(define (matrix-translate-make x y)
  (list 1.0 0.0 0.0 1.0 x y))

;; rotate

;; cos  sin 0
;; -sin cos 0
;; 0    0   1

;; (cos sin -sin cos 0 0)

(define (matrix-rotate-make z)
  (let ((c (cos z))
        (s (sin z))
        (ns (- (sin z))))
    (list c s ns c 0.0 0.0)))

;; to string

(define (matrix-to-concatenate-string matrix)
  (format #f "(concatenate-matrix ~a)" (matrix-to-string matrix)))

(define (matrix-to-string matrix)
  (format #f "~a ~a ~a ~a ~a ~a" (first matrix) (second matrix)
          (third matrix) (fourth matrix) (fifth matrix) (sixth matrix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matrix Concatenation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; concatenate

;; Multiply the rows of A by the columns of B
;;(define a (matrix-translate-make 10 20))(define b (matrix-translate-make 200 100))(define c (matrix-concatenate a b)) c

;; This is going to be a bottleneck, so unroll it
;; Optimise so rather than first, second, third, we use car/cdr on each cdr

(define (matrix-concatenate a b)
  (let* ((aa (first a))
         (ab (second a))
         (ac (third a))
         (ad (fourth a))
         (ae (fifth a))
         (af (sixth a))
         (ba (first b))
         (bb (second b))
         (bc (third b))
         (bd (fourth b))
         (be (fifth b))
         (bf (sixth b)))
    (list (+ (* aa ba) (* ab bc)) ;;(* 0.0 btx)
          (+ (* aa bb) (* ab bd)) ;;(* 0.0 b32)
          ;;(+ (* a11 b13) (* a12 b23) (* a13 b33))
          (+ (* ac ba) (* ad bc)) ;;(*a23 b31)
          (+ (* ac bb) (* ad bd)) ;;(* a23 b32)
          ;;(+ (* a21 b13) (* a22 b23) (* a23 b33))
          (+ (* ae ba) (* af bc) be) ;;(* a33 b31)
          (+ (* ae bb) (* af bd) bf)))) ;;(* a33 b32)
;;(+ (* a31 b13) (* a32 b23) (* a33 b33))

;; concatenaten
;; Concatenate a list of matrices

(define (matrix-concatenaten a . ms)
  (let ((product (matrix-concatenate a (car ms)))
        (rest (cdr ms)))
    (if (not rest)
        product
        (matrix-concatenaten product (cdr ms)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying transformation matrices to objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; transform
;; Transform a point by a matrix
;; This is going to be a bottleneck, so unroll it
;; Optimise so rather than first, second, third, we use car/cdr on each cdr

(define (matrix-point-transform x y m)
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
    (values xx yy)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matrix stacks, pushing, popping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-matrix-stack)
  (list (matrix-identity-make)))

(define (stack-set-matrix matrix-stack matrix)
  (cons matrix (cdr matrix-stack)))

(define (stack-concatenate-matrix matrix-stack matrix)
  (stack-set-matrix matrix-stack
                    (matrix-concatenate matrix
                                        (stack-current-matrix matrix-stack))))

(define (stack-current-matrix matrix-stack)
  (car matrix-stack))

(define (stack-push-matrix matrix-stack)
  (cons (copy-tree (stack-current-matrix matrix-stack))
        matrix-stack))

(define (stack-pop-matrix matrix-stack)
  (cdr matrix-stack))

(define (get-translate-values trans)
  (let* ((tokens (string-tokenize trans (string->char-set "0123456789.-")))
         (x (first tokens))
         (y (second tokens)))
    (values (string->number x) (string->number y))))
