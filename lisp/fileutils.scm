;;; ttn/fileutils.scm --- Munge filenames

;; Rel:v-0-44-raggiungere-la-cima
;;
;; Copyright (C) 2001-2002,2004-2005 Thien-Thi Nguyen
;; This file is part of ttn's personal scheme library, released under GNU
;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Commentary:

;; This modules exports four procs:
;;  (filename-absolute-or-in-vicinity name vicinity) => string
;;  (filename-sans-end-slash name) => string
;;  (filename-components string) => list
;;  (filename-components-append ls) => string
;;
;; These all deal with filenames, so the module should probably be renamed
;; to filenameutils or somesuch.  Oh well.

;;; Code:

(define-module (ttn fileutils)
  #:autoload (scripts split-string-no-nulls) (split-string-no-nulls)
  #:autoload (srfi srfi-13) (string-take-right string-join)
  #:export (filename-absolute-or-in-vicinity
            filename-sans-end-slash
            filename-components
            filename-components-append))

;; If @var{name} begins with "/", return it.  Otherwise, return a new string
;; composed by taking @var{name} in vicinity of @var{dir}.
;;
(define (filename-absolute-or-in-vicinity name dir)
  (or (and (not (string-null? name))
           (char=? #\/ (string-ref name 0))
           name)
      (in-vicinity dir name)))

;; Return @var{name}, a string, stripping the terminating "/" character.
;; If there is no "/", just return @var{name}.
;;
(define (filename-sans-end-slash name)
  (if (string=? "/" (string-take-right name 1))
      (string-drop-right name 1)
      name))

;; Return a list of filename components parsed from @var{string}.
;; Components are delimited by "/", which is discarded.
;; Null string components are also discarded.
;;
(define (filename-components string)
  (split-string-no-nulls string "/"))

;; Return a string composed by prefixing each element of @var{ls} with "/".
;;
(define (filename-components-append ls)
  (string-join ls "/" 'prefix))

;;; ttn/fileutils.scm ends here
