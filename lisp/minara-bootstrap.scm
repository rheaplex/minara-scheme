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

;; Load the global application configuration file installed on this machine

(define (load-config)
  (if (access? "./minara-config.scm" R_OK)
      (load "./minara-config.scm")))

;; Load the user's ~/.minara file stored in their home directory

(define (load-user-config)
  (if (access? "~/.minara" R_OK)
      (load "~/.minara")))

;; Load the libraries in the correct order

(define (load-libraries)
;;  (define dir (opendir "/usr/lib"))
;;  (do ((entry (readdir dir) (readdir dir)))
;;      ((eof-object? entry))
;;    (display entry)(newline))
;;  (closedir dir))
  (load "./test.scm")
  (load "./transformations.scm")
  (load "./rendering.scm")
  (load "./events.scm")
  (load "./keymap.scm")
  (load "./buffer.scm")
  (load "./window.scm")
  (load "./command-line.scm")
  (load "./menu.scm")
  (load "./geometry.scm")
  (load "./picking.scm")
  (load "./tool.scm")
  (load "./view-tools.scm")
  (load "./undo.scm"))

;; Load the tools

(define (load-tools)
  (load "./pen-tool.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Our main startup routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (startup args)
  (debug-enable  'debug 'backtrace)
  (trace)
  ;; Load the application configuration file
  (load-config)
  ;; Load the user config file
  (load-user-config)
  ;; Load the libraries
  (load-libraries)
  ;; Load the tools
  (load-tools)
  ;; Get the event hooks back into C in case anyone forgot to
  (bind-event-hooks)
  ;; Handle the command line
  (cli-handle-arguments))

;; Call startup

(startup (program-arguments))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run the tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(run-tests)
