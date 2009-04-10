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
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (minara sexp)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-11)
  :use-module (srfi srfi-13)
  :use-module (minara buffer)
  :use-module (minara window)
  :export (nth-occurrence
	   sexp-bounds
	   reverse-sexp-bounds
	   nth-sexp-bounds
	   get-nth-sexp
	   get-nth-path
	   sexp-before
	   sexp-after
	   sexp-symbol-string))
  
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
    (values start end)))

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

;; Get an s-expression from the ) at the character index given to the opening (
(define (reverse-sexp-bounds buffer start)
  (let ((end (sexp-bounds-aux buffer (- start 1) 1)))
    (values start end)))

;; Recursively find the beginning of the s-expression
(define (reverse-sexp-bounds-aux buffer current count)
  ;; Terminal clause, return the value
  (if (= count 0)
      current
      ;; Otherwise we get the current char and check it
      (let ((current-char (substring buffer current (- current 1))))
	(cond
	  ((string= current-char ")")
	   (sexp-bounds-aux buffer (- current 1) (+ count 1)))
	  ((string= current-char "(") 
	   (sexp-bounds-aux buffer (- current 1) (- count 1)))
	  (else
	   (sexp-bounds-aux buffer (- current 1) count))))))

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
    (let ((path-from (- (nth-occurrence buffer
				      "(path-begin)" nth)
			1))
	   ;; 10 to move past "path-end"
	   (path-to (+ (nth-occurrence buffer 
				       "(path-end)" nth) 10)))
      (values path-from path-to)))

(define (sexp-before buffer-str pos)  
    (let ((sexp-start (string-rindex buffer-str #\( 0 pos)))
      (if sexp-start
	  (sexp-bounds buffer-str sexp-start)
	  #f)))

(define (sexp-after buffer-str pos)
    (let ((sexp-start (string-index buffer-str #\( pos)))
      (if sexp-start
	  (sexp-bounds buffer-str sexp-start)
	  #f)))

(define (sexp-symbol-string buffer-str sexp-pos)
    (if (string= (substring buffer-str sexp-pos (+ sexp-pos 1))
		 "(")
	(let ((symbol-end (or (string-index buffer-str #\space sexp-pos)
			      (string-index buffer-str #\) sexp-pos))))
	  (if symbol-end
	      (substring buffer-str 
			 (+ sexp-pos 1 ) 
			 symbol-end)
	      #f))
	#f))
