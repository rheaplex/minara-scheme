;;; ttn/find-file.scm --- make a buffer visit a file

;; Rel:v-0-37-sempre-SUDRA
;;
;; Copyright (C) 2001-2002,2004 Thien-Thi Nguyen
;; This file is part of ttn's personal scheme library, released under GNU
;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (ttn find-file)
  #:autoload (ttn gap-buffer) (make-gap-buffer gb-insert-string!)
  #:autoload (scripts slurp) (slurp)
  #:autoload (ttn expand-file-name) (expand-file-name)
  #:export (find-file))

;; Insert file named @var{filename} into a buffer, and return that buffer.
;; @var{filename} is passed through @code{expand-file-name}, q.v.
;; Optional second arg @var{buffer} specifies a buffer to use,
;; default is to create and return a new one.
;; If the file is not readable, return #f.
;; This causes the buffer to visit the file.
;;
;;-sig: (filename [buffer])
;;
(define (find-file filename . buffer)
  (let ((filename (expand-file-name filename)))
    (and (access? filename R_OK)
         (let ((buf (if (null? buffer)
                        (make-gap-buffer)
                        (car buffer))))
           (gb-insert-string! buf (slurp filename))
           (set-object-property! buf 'filename filename)
           (gb-goto-char buf (gb-point-min buf))
           buf))))

;;; ttn/find-file.scm ends here
