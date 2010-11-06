(define-module (minara-tests buffer)
  :use-module (oop goops)
  :use-module (unit-test)
  :use-module (minara buffer))

(define-class <test-buffer> (<test-case>)
  (buff #:accessor buffer))

(define-method (set-up-test (self <test-buffer>))
  (set! (buffer self) (make-buffer)))

;; Buffer Variables

(define-method (test-buffer-variables (self <test-buffer>))
  ;; set-buffer-variable!
  (set-buffer-variable! (buffer self) "a" 100)
  (assert-equal 100 (buffer-variable (buffer self) "a"))
  ;; kill-buffer-variable!
  (kill-buffer-variable! (buffer self) "a")
  (assert-equal #f (buffer-variable (buffer self) "a")))

(define-method (test-buffer-text (self <test-buffer>))
  (assert-equal "" (buffer-to-string (buffer self)))
  ;; buffer-insert...
  (buffer-insert-no-undo (buffer self) 0 "category")
  (assert-equal "category" (buffer-to-string (buffer self)))
  ;; buffer-start
  (assert-equal 1 (buffer-start (buffer self)))
  ;; buffer-end
  (assert-equal 9 (buffer-end (buffer self)))
  ;; buffer-range-to-string
  (assert-equal "cat" (buffer-range-to-string (buffer self) 1 4))
  (assert-equal "tego" (buffer-range-to-string (buffer self) 3 7))
  (assert-equal "gory" (buffer-range-to-string (buffer self) 5 9))
  ;; More buffer-insert...
  (buffer-insert-no-undo (buffer self) #f "train")
  (assert-equal "categorytrain" (buffer-to-string (buffer self)))
  (buffer-insert-no-undo (buffer self) 0 "cake")
  (assert-equal "cakecategorytrain" (buffer-to-string (buffer self)))
  (buffer-insert-no-undo (buffer self) 5 "oscillation")
  (assert-equal "cakeoscillationcategorytrain" (buffer-to-string (buffer self)))
  ;; buffer-erase!
  (buffer-erase (buffer self))
  (assert-equal "" (buffer-to-string (buffer self))))