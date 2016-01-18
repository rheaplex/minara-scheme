;; cairo-rendering.scm : graphics rendering with cairo for minara
;;
;; Copyright (c) 2016 Rob Myers, rob@robmyers.org
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
;; cairo-rendering
;; The minara graphics protocol implemented in cairo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (minara-internal cairo-rendering)
  :use-module (cairo)
  :export (initialise-protocol
           finalise-protocol
           path-begin
           path-end
           move-to
           line-to
           curve-to
           set-rgba
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
           rotate))

;; These should go in some sort of context state bundle
(define $context #f)
(define $context-matrix-stack #f)

(define (initialise-protocol context width height)
  (set! $context context)
  (set! $context-matrix-stack '())
  (cairo-identity-matrix $context)
  (cairo-translate context 0 height)
  (cairo-scale context 1.0 -1.0)
  (cairo-set-source-rgb context 1 1 1)
  (cairo-paint context))

(define (finalise-protocol)
  (set! $context #f)
  (set! $context-matrix-stack #f))

(define (path-begin)
  (cairo-new-path $context))

(define (path-end)
  (cairo-close-path $context)
  (cairo-fill $context))

(define (move-to x y)
  (cairo-move-to $context x y))

(define (line-to x y)
  (cairo-line-to $context x y))

(define (curve-to x1 y1 x2 y2 x3 y3)
  (cairo-curve-to $context x1 y1 x2 y2 x3 y3))

(define (set-rgba r g b a)
  (cairo-set-source-rgba $context r g b a))

(define (begin-mask)
  #f)

(define (end-mask)
  #f)

(define (begin-masking)
  #f)

(define (end-masking)
  #f)

(define (push-matrix)
  (set! $context-matrix-stack
        (append (list (cairo-get-matrix $context)) $context-matrix-stack)))

(define (pop-matrix)
  (let ((popped (car $context-matrix-stack)))
    (cairo-set-matrix $context popped)
    (set! $context-matrix-stack (cdr $context-matrix-stack))))

(define (concatenate-matrix m11 m12 m21 m22 m31 m32)
  #f)

(define (set-matrix m11 m12 m21 m22 m31 m32)
  #f)

(define (identity-matrix)
  (cairo-identity-matrix $context))

(define (translate tx ty)
  (cairo-translate $context tx ty))

(define (scale sx sy)
  (cairo-scale $context sx sy))

(define (rotate r)
  (cairo-rotate $context r))
