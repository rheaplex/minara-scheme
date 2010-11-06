(define-module (minara-tests picking)
  :use-module (oop goops)
  :use-module (unit-test)
  :use-module (minara picking)
  :use-module (minara picking-hit)
  :use-module (minara buffer))

(define-class <test-picking> (<test-case>)
  (frag #:accessor fragment))

(define-method (set-up-test (self <test-picking>))
  (set! (fragment self)
	(make-buffer-from-string ";;minara file\n(set-colour 0.0 0.0 1.0 1.0)\n(path-begin)\n(move-to 10 10)\n(line-to 10 100)\n(line-to 100 10)\n(line-to 10 10)\n(path-end)\n")))

(define-method (test-picking (self <test-picking>))
  ;; Hit
  (let ((hit (car (pick-paths (fragment self) 50 50))))
    (assert-equal 43 (picking-hit-from hit))
    (assert-equal 133 (picking-hit-to hit))
    (assert-equal '(1.0 0.0 0.0 1.0 0.0 0.0) (picking-hit-transform hit)))
  ;; Miss
  (let ((hits (pick-paths (fragment self) 5000 5000)))
    (assert-equal #f hits)))
