;;; ttn/write-buffer-if-changed.scm --- Avoid unnecessary mtime modifcation

;; Rel:v-0-37-sempre-SUDRA
;;
;; Copyright (C) 2001-2002,2004 Thien-Thi Nguyen
;; This file is part of ttn's personal scheme library, released under GNU
;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (ttn write-buffer-if-changed)
  #:autoload (ttn gap-buffer) (gb-point-max gb->string)
  #:autoload (scripts slurp) (slurp)
  #:autoload (ttn write-buffer) (write-buffer)
  #:export (write-buffer-if-changed))

;; Write @var{buffer} into file @var{filename}, but only if doing so would
;; create a new file or a file with different contents than the pre-existing
;; one.  @var{filename} is passed through @code{expand-file-name}, q.v.
;; @var{filename} cannot be a directory.
;;
(define (write-buffer-if-changed buffer filename)
  (and (or (not (file-exists? filename))
           (not (= (1- (gb-point-max buffer)) (stat:size (stat filename))))
           (not (string=? (gb->string buffer) (slurp filename))))
       (write-buffer buffer filename)))

;;; ttn/write-buffer-if-changed.scm ends here
