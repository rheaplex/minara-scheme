(define-module (minara-tests buffer)
  :use-module (oop goops)
  :use-module (unit-test)
  :use-module (minara buffer))

(define-class <test-buffer> (<test-case>)
  (buff #:accessor buffer))

(define-method (set-up-test (self <test-buffer>))
  (set! (buffer self) (make-buffer)))

(define-method (test-buffer-basics (self <test-buffer>))
  (let* ((src "0123456789")
	 (buf (make-buffer-from-string src))
	 (buffer-length (string-length src))
	 (empty-buf (make-buffer)))
    ;; Empty buffer is empty, starting at first index and finishing after it
    (assert-equal 1 (buffer-start empty-buf))
    (assert-equal 1 (buffer-end empty-buf))
    ;; Sanity check. Make sure the buffer contains the source string
    (assert-equal src (buffer-to-string buf))
    ;; Buffer indexes start at 1
    (assert-equal 1 (buffer-start buf))
    ;; Buffer length is (# chars in buffer + 1)
    (assert-equal (+ buffer-length 1) (buffer-end buf))
    ;; And we can access the first and last characters OK
    (assert-equal "0" (buffer-range-to-string buf 1 2))
    (assert-equal "9" (buffer-range-to-string buf buffer-length
					      (+ buffer-length 1)))))

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