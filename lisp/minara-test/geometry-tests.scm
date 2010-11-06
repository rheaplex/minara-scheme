(define-module (minara-tests geometry)
  :use-module (minara geometry)
  :use-module (unit-test)
  :use-module (oop goops))

(define-class <test-geometry> (<test-case>))

(define-method (test-intersection (self <test-geometry>))
  (assert-equal 0.0 (lines-intersect-vertices 0.0 0.0 0.0 100.0
					      0.0 0.0 100.0 0.0))
  (assert-equal 0.5 (lines-intersect-vertices 0.0 0.0 0.0 100.0 
					      0.0 50.0 100.0 50.0))
  (assert-equal 1.0 (lines-intersect-vertices 0.0 0.0 0.0 100.0 
					      0.0 100.0 100.0 100.0))
  (assert-equal 0.5 (lines-intersect-vertices 0.0 0.0 100.0 100.0 
					      0.0 50.0 100.0 50.0))
  (assert-equal #f (lines-intersect-vertices 0.0 0.0 100.0 100.0 
					     1000.0 1000.0 1000.0 1000.0))
  (assert-equal 0 (line-bezier-intersection-count-vertices 20 0 80 0
							   0 0 0 100
							   100 100 100 0))
  ;; Aligned with end-point of a subdivision (given t step of 0.1)
  (assert-equal 1 (line-bezier-intersection-count-vertices 50 0 50 150
							   0 0 0 100
							   100 100 100 0))
  ;; Not aligned with end-point of subdivision (given t step of 0.1)
  (assert-equal 1 (line-bezier-intersection-count-vertices 52 0 52 150
							   0 0 0 100
							   100 100 100 0))
  (assert-equal 2 (line-bezier-intersection-count-vertices 0 50 100 50
							   0 0 0 100
							   100 100 100 0)))
