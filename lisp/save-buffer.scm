;;; ttn/save-buffer.scm --- save visting buffer to disk

;; Rel:v-0-37-sempre-SUDRA
;;
;; Copyright (C) 2001-2002,2004 Thien-Thi Nguyen
;; This file is part of ttn's personal scheme library, released under GNU
;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (ttn save-buffer)
  #:autoload (ttn gap-buffer) (gb->port!)
  #:export (save-buffer))

;; Save @var{buffer} to disk.  @var{buffer} should be visiting a file.
;; An error is thrown if @var{buffer} is not visiting a file.
;; An error is thrown if the file cannot be written.
;;
;; Return #t if all goes well.
;;
(define (save-buffer buffer)
  (let ((filename (or (object-property buffer 'filename)
                      (error "buffer `filename' not set:" buffer))))
    (or (access? filename W_OK)
        (error "file not writeable:" filename))
    (let ((p (open-output-file filename)))
      (gb->port! buffer p)
      (force-output p)
      (close-port p))
    (= (stat:size (stat filename))
       (1- (gb-point-max buffer)))))

;;; ttn/save-buffer.scm ends here
