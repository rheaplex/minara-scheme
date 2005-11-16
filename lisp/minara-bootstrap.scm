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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrapping
;; Find and load all the Scheme code for minara in the correct order.
;; We don't handle dependencies at the moment.
;; Loading the libraries, then the tools seems to work well, though.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; getopt-long...
(use-modules (ice-9 getopt-long) (ice-9 debug))

;; Load third party libraries

(load "./slurp.scm")
(load "./split-string-no-nulls.scm")
(load "./fileutils.scm")
(load "./expand-file-name.scm")

(load "./gap-buffer.scm")
(load "./find-file.scm")
(load "./write-buffer.scm")


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

(define $library-files
    '("test"
      "transformations"
      "rendering"
      "test"
      "transformations"
      "rendering"
      "events"
      "keymap"
      "buffer"
      "window"
      "command-line"
      "menu"
      "geometry"
      "picking"
      "tool"
      "undo"
      "development"))

(define $tool-files
    '("view-tools"
      "colour-tools"
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

(define (load-libraries)
    (load-files $minara-lisp-dir
		$library-files))

;; Load the tools

(define (load-tools)
    (load-files $minara-lisp-dir
		$tool-files))


(define (load-minara-files)
  ;; Load the libraries
  (load-libraries)
  ;; Load the tools
  (load-tools)
  ;; Load the user config file
  (load-user-config))

(define (load-and-initialise)
    (load-minara-files)
  ;; Get the event hooks back into C in case anyone forgot to
  (bind-event-hooks))
  
(define $minara-lisp-dir
    (if (string=? (utsname:machine (uname))
		  "Power Macintosh")
	"../Resources/lisp/"
	"../lisp/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Our main startup routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (startup args)
  (debug-enable 'debug) 
  (debug-enable 'backtrace)
  (read-enable 'positions)
  (load-and-initialise)
  ;; Handle the command line
  (cli-handle-arguments))

;; Call startup

(startup (program-arguments))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run the tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(run-tests)
