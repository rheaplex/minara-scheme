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
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; getopt-long...
(use-modules (ice-9 getopt-long))

;; Load third party libraries

(load "./slurp.scm")
(load "./split-string-no-nulls.scm")
(load "./fileutils.scm")
(load "./expand-file-name.scm")
(load "./gap-buffer.scm")
(load "./find-file.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the global application configuration file

(define (load-config)
  (if (access? "./minara-config.scm" R_OK)
      (load "./minara-config.scm")))

;; Load the user's ~/.minara file

(define (load-user-config)
  (if (access? "~/.minara" R_OK)
      (load "~/.minara")))

;; Load the libraries

(define (load-libraries)
;;  (define dir (opendir "/usr/lib"))
;;  (do ((entry (readdir dir) (readdir dir)))
;;      ((eof-object? entry))
;;    (display entry)(newline))
;;  (closedir dir))
  (load "./test.scm")
  (load "./rendering.scm")
  (load "./events.scm")
  (load "./keymap.scm")
  (load "./buffer.scm")
  (load "./command-line.scm")
  (load "./menu.scm")
  (load "./picking.scm")
  (load "./tool.scm")
  (load "./undo.scm")
  (load "./pen-tool.scm"))

;; Load the tools

(define (load-tools)
  #f)

;; Our main startup

(define (startup args)
  (debug-enable 'debug 'backtrace)
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

;; Run the tests

(run-tests)