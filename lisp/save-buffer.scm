;;; ttn/save-buffer.scm --- save visting buffer to disk

;; Rel:v-0-32-che-caldo
;;
;; Copyright (C) 2001-2002 Thien-Thi Nguyen
;; This file is part of ttn's personal scheme library, released under GNU
;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Commentary:

;; This module exports the proc `save-buffer':
;;   (save-buffer buffer)
;;
;; Save BUFFER to disk.  BUFFER should be visiting a file.
;; An error is thrown if BUFFER is not visiting a file.
;; An error is thrown if the file cannot be written.
;;
;; Return #t if all goes well.

;;; Code:

(define-module (ttn save-buffer)
  :autoload (ttn gap-buffer) (gb->port!))

(define (save-buffer buffer)
  (let ((filename (or (object-property buffer 'filename)
                      (error "buffer `filename' not set:" buffer))))
    (or (access? filename W_OK)
        (error "file not writeable:" filename))
    (gb->port! buffer (open-output-file filename))
    (flush-all-ports)
    (= (stat:size (stat filename))
       (1- (gb-point-max buffer)))))

(export save-buffer)

;;; ttn/save-buffer.scm ends here
