;;; ttn/fileutils.scm --- Munge files in core

;; Rel:v-0-32-che-caldo
;;
;; Copyright (C) 2001-2002 Thien-Thi Nguyen
;; This file is part of ttn's personal scheme library, released under GNU
;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Commentary:

;; This module exports the procs:
;;   (filename-sans-end-slash name)
;;   (filename-components string)
;;   (filename-components-append ls)

;;; Code:

(define-module (ttn fileutils)
  :autoload (scripts split-string-no-nulls) (split-string-no-nulls)
  :autoload (srfi srfi-13) (string-take-right string-join))

;; Return NAME, a string, stripping the terminating "/" character.
;; If there is no "/", just return NAME.
;;
(define (filename-sans-end-slash name)
  (if (string=? "/" (string-take-right name 1))
      (string-drop-right name 1)
      name))

;; Return a list of filename components parsed from STRING.
;; Components are delimited by "/", which is discarded.
;; Null string components are also discarded.
;;
(define (filename-components string)
  (split-string-no-nulls string "/"))

;; Return a string composed by prefixing each element of LS with "/".
;;
(define (filename-components-append ls)
  (string-join ls "/" 'prefix))

(export filename-sans-end-slash
        filename-components
        filename-components-append)

;;; ttn/fileutils.scm ends here
