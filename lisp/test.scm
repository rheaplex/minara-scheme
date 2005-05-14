;; test.scm : minara scheme development file
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
;; Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Store all the tests as they are defined
(define *tests* '())
;; The total number of tests run
(define *tests-run* 0)
;; And the total number of tests failed
(define *tests-failed* 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A single test assertion
(define-macro (test-assert comparison comment expected code)
  `(set! *tests*
	 (cons
	  (lambda ()
	    (set! *tests-run* (+ *tests-run* 1))
	    (let ((result ,code))
	      (if (not (,comparison ,expected result))
		  (begin
		    (format #t "~a: " ,comment)
		    (write ',code)
		    (format #t "~%  Expected ~a, got ~a.~%" ,expected result)
		    (set! *tests-failed* (+ *tests-failed* 1))))))
	  *tests*)))

(define-macro (test expected code)
  `(test-assert equal? "Test unexpectedly failed" ,expected ,code))

(define-macro (test-fail expected code)
  `(test-assert (lambda (a b) (not (equal? a b))) 
		"Test unexpectedly succeeded" ,expected ,code))

(define-macro (test-section name)
  `(set! *tests*
	 (cons
	  ,name
	  *tests*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Run all the tests
(define (run-tests)
    (format #t "Running tests.~%")
    (run-tests-aux (reverse *tests*))
    (format #t "Total tests passed: ~a/~a~%" 
	    (- *tests-run* *tests-failed*) 
	    *tests-run*))

(define (run-tests-aux tests)
  (if (not (null? tests))
      (begin
	(if (string? (car tests))
	    (format #t "---->~a~%" (car tests))
	    ((car tests)))
	(run-tests-aux (cdr tests)))))