;; minara-bootstrap.scm : load the support files and call startup
;;
;; Copyright (c) 2004 Rob Myers, rob@robmyers.org
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(use-modules (minara-internal config) (minara-internal events))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Scheme files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the user's ~/.minara file stored in their home directory

(define (load-user-config)
    (let ((config (string-append (getenv "HOME")
				 "/.minara")))
      (if (access? config R_OK)
	  (primitive-load config))))

;; Load the libraries in the correct order

(define $third-party-files
  '("slurp"
    "split-string-no-nulls"
    "gap-buffer"))

(define $library-files
    '("test"
      "transformations"
      "rendering"
      "keymap"
      "buffer"
      "window"
      "events"
      "command-line"
      "menu"
      "geometry"
      "tool"
      "view"
      "sexp"
      "picking-hit"
      "picking-protocol"
      "picking"
      "undo"
      "selection"
      "cut-and-paste"
      "minibuffer"
      "development"))

(define $tool-files
    '("colour-tools"
      "pen-tool"
      "shape-tools"))

(define (load-file path file)
    (primitive-load (string-append path
			 "/"
			 file
			 ".scm")))

(define (load-files path files)
    (for-each (lambda (file)
		(load-file path file))
	      files))

(define (load-third-party-files)
    (load-files $minara-lisp-dir
		$third-party-files))

(define (load-libraries)
    (load-files $minara-lisp-dir
		$library-files))

(define (load-tools)
    (load-files $minara-lisp-dir
		$tool-files))

(define (load-minara-files)
  (load-third-party-files)
  (load-libraries)
  (load-tools)
  (load-user-config))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main startup code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(read-enable 'positions)
(load-minara-files)
(bind-event-hooks)

(use-modules (minara command-line))
(cli-handle-arguments)