;;; ttn/expand-file-name.scm --- convert filename to absolute canonical form

;; Rel:v-0-32-che-caldo
;;
;; Copyright (C) 2001-2002 Thien-Thi Nguyen
;; This file is part of ttn's personal scheme library, released under GNU
;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Commentary:

;; This module exports the proc `expand-file-name':
;;   (expand-file-name name . default-directory)
;;
;; Convert filename NAME to absolute, and canonicalize it.
;; Second arg DEFAULT-DIRECTORY is directory to start with if NAME is relative
;;  (does not start with slash); if DEFAULT-DIRECTORY is #f or missing,
;;  `(getcwd)' is used.
;; File name components that are `.' are removed, and
;; so are file name components followed by `..', along with the `..' itself;
;; note that these simplifications are done without checking the resulting
;; file names in the file system.
;; An initial `~/' expands to your home directory.
;; An initial `~USER/' expands to USER's home directory.

;;; Code:

(define-module (ttn expand-file-name)
  :use-module ((ttn fileutils)
               :select (filename-sans-end-slash
                        (filename-components . comp-sep)
                        (filename-components-append . comp-append))))

(define concat string-append)
(define subs   substring)

(define (elide-dot-and-dot-dots abs-name)
  (let loop ((comps (reverse (comp-sep abs-name)))
             (omit 0)
             (acc '()))
    (if (null? comps)
        (comp-append acc)
        (cond ((string=? "." (car comps))
               (loop (cdr comps) omit acc))
              ((string=? ".." (car comps))
               (loop (cdr comps) (1+ omit) acc))
              ((< 0 omit)
               (loop (cdr comps) (1- omit) acc))
              (else
               (loop (cdr comps) omit (cons (car comps) acc)))))))

(define (expand-file-name name . default-directory)
  (let* ((fc (lambda (s) (string-ref s 0)))
         (dd (lambda () (concat (filename-sans-end-slash
                                 (or (and (not (null? default-directory))
                                          (car default-directory))
                                     (getcwd)))
                                "/")))
         (len (string-length name)))
    (filename-sans-end-slash
     (elide-dot-and-dot-dots
      (cond ((char=? (fc name) #\/) name)
            ((char=? (fc name) #\~)
             (let ((end (or (string-index name #\/) len)))
               (concat (cond ((= 1 end)
                              (passwd:dir (getpwuid (getuid))))
                             ((false-if-exception
                               (passwd:dir (getpwnam (subs name 1 end)))))
                             (else
                              (concat (dd) (subs name 0 end))))
                       (subs name end len))))
            (else
             (concat (dd) name)))))))

(export expand-file-name)

;;; ttn/expand-file-name.scm ends here
