;;; ttn/write-buffer.scm --- write buffer to file and visit it

;; Rel:v-0-37-sempre-SUDRA
;;
;; Copyright (C) 2001-2002,2004 Thien-Thi Nguyen
;; This file is part of ttn's personal scheme library, released under GNU
;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (ttn write-buffer)
  #:use-module ((ttn expand-file-name) #:select (expand-file-name))
  #:use-module ((ttn gap-buffer)     #:select (gb->string))
  #:export (write-buffer))

;; Write @var{buffer} into file @var{filename}.
;; @var{filename} is passed through @code{expand-file-name}, q.v.
;; This makes the buffer visit that file.
;; @var{filename} cannot be a directory.
;;
(define (write-buffer buffer filename)
  (let ((filename (expand-file-name filename)))
    (and (file-exists? filename)
         (file-is-directory? filename)
         (error "invalid filename (names a directory)"))
    (set-object-property! buffer 'filename filename)
    (with-output-to-file filename
      (lambda () (display (gb->string buffer))))))

;;; ttn/write-buffer.scm ends here
