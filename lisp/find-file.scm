;;; ttn/find-file.scm --- make a buffer visit a file

;; Rel:v-0-32-che-caldo
;;
;; Copyright (C) 2001-2002 Thien-Thi Nguyen
;; This file is part of ttn's personal scheme library, released under GNU
;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Commentary:

;; This module exports the proc `find-file':
;;   (find-file filename . buffer)
;;
;; Insert file named FILENAME into a buffer, and return that buffer.
;; FILENAME is passed through `expand-file-name', q.v.
;; Optional second arg BUFFER specifies a buffer to use,
;; default is to create and return a new one.
;; If the file is not readable, return #f.
;; This causes the buffer to visit the file.

;;; Code:

(define-module (ttn find-file)
  :autoload (scripts slurp) (slurp)
  :autoload (ttn expand-file-name) (expand-file-name)
  :autoload (ttn gap-buffer) (make-gap-buffer gb-insert-string!))

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

(export find-file)

;;; ttn/find-file.scm ends here
