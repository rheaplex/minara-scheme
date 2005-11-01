;;; ttn/expand-file-name.scm --- convert filename to absolute canonical form

;; Rel:v-0-42-corrente-staccata
;;
;; Copyright (C) 2001-2002,2004-2005 Thien-Thi Nguyen
;; This file is part of ttn's personal scheme library, released under GNU
;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Commentary:

;; This module exports four procs:
;;   (expand-file-name name [default-directory])
;;   (reset-tilde-cache! [size])
;;   (substitute-env-vars string)
;;   (expand-file-name-substituting-env-vars name [default-directory])

;;; Code:

(define-module (ttn expand-file-name)
  #:autoload (ice-9 regex) (match:start match:substring match:end)
  #:use-module ((ttn fileutils)
                #:select ((filename-components . comp-sep)
                          (filename-components-append . comp-append)))
  #:export (expand-file-name
            reset-tilde-cache!
            substitute-env-vars
            expand-file-name-substituting-env-vars))

(define concat string-append)
(define subs   substring)

(define (elide-dot-and-dot-dots abs-name need-trailing-sep?)
  (let loop ((comps (reverse! (comp-sep abs-name)))
             (omit 0)
             (acc (if need-trailing-sep? ; blech
                      (list "")
                      (list))))
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

(define *tilde-cache* 7)

;; Convert filename @var{name} to absolute, and canonicalize it.
;; Second arg @var{default-directory} is directory to start with if @var{name}
;; is relative (does not start with slash); if @var{default-directory} is #f
;; or missing, @code{(getcwd)} is used.
;; File name components that are @file{.} are removed, and so are file name
;; components followed by @file{..}, along with the @file{..} itself;
;; note that these simplifications are done without checking the resulting
;; file names in the file system.
;; An initial @file{~/} expands to your home directory.
;; An initial @file{~USER/} expands to USER's home directory.
;;
;;-sig: (name [default-directory])
;;
(define (expand-file-name name . default-directory)
  (and (number? *tilde-cache*)
       (set! *tilde-cache* (make-hash-table *tilde-cache*)))
  (let* ((~? (lambda (key pw-ent-thunk)
               (or (hash-ref *tilde-cache* key)
                   (let ((val (passwd:dir (pw-ent-thunk))))
                     (hash-set! *tilde-cache* key val)
                     val))))
         (dd (or (and (pair? default-directory)
                      (car default-directory))
                 (getcwd)))
         (len (string-length name)))
    (elide-dot-and-dot-dots
     (case (string-ref name 0)
       ((#\/) name)
       ((#\~) (let ((end (or (string-index name #\/) len)))
                (concat (cond ((= 1 end)
                               (~? "~" (lambda () (getpwuid (getuid)))))
                              ((false-if-exception
                                (let ((user (subs name 1 end)))
                                  (~? user (lambda () (getpwnam user))))))
                              (else
                               (in-vicinity dd (subs name 0 end))))
                        (subs name end len))))
       (else (in-vicinity dd name)))
     (char=? #\/ (string-ref name (1- len))))))

;; Reset the cache @code{expand-file-name} uses for the results
;; of expanding @code{~} and @code{~USER}.  Optional arg @var{size}
;; specifies the hash table bucket count to use (default is 7).
;;
;;-sig: ([size])
;;
(define (reset-tilde-cache! . size)
  (set! *tilde-cache* (or (and (pair? size)
                               (number? (car size))
                               (car size))
                          7)))

;; Substitute environment variables referred to in @var{string}.
;; @code{$FOO} where FOO is an environment variable name means to substitute
;; the value of that variable.  The variable name should be terminated
;; with a character not a letter, digit or underscore; otherwise, enclose
;; the entire variable name in braces.  For instance, in @code{ab$cd-x},
;; @code{$cd} is treated as an environment variable.
;;
;; Use @code{$$} to insert a single dollar sign.
;;
(define substitute-env-vars
  (let ((double (make-regexp "[$][$]"))
        (rx (make-regexp "[$][{]*([A-Za-z0-9_][A-Za-z0-9_]*)[}]*"))
        (sub make-shared-substring))
    (lambda (string)
      (let ((answer (list #f)))
        (let loop ((start 0) (tp answer))
          (cond ((regexp-exec double string start)
                 => (lambda (m)
                      (let ((one (1+ (match:start m))))
                        (set-cdr! tp (list (sub string start one)))
                        (loop (1+ one) (cdr tp)))))
                ((regexp-exec rx string start)
                 => (lambda (m)
                      (let ((var (match:substring m 1)))
                        (set-cdr! tp (list (sub string start (match:start m))
                                           (or (getenv var) "")))
                        (loop (match:end m) (cddr tp)))))
                (else
                 (set-cdr! tp (list (sub string start))))))
        (apply string-append (cdr answer))))))

;; Substitute env vars in @var{string} then expand it as a filename.
;; See @code{substitute-env-vars} and @code{expand-file-name}.
;;
;;-sig: (name [default-directory])
;;
(define (expand-file-name-substituting-env-vars name . rest)
  (apply expand-file-name (substitute-env-vars name) rest))

;;; ttn/expand-file-name.scm ends here
