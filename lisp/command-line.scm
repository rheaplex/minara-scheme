;; command-line.scm : minara scheme development file
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

(define (cli-help)
  (write-line "Usage: minara [OPTION]...")
  (write-line "")
  (write-line "Run minara, a programmable graphics program editor.")
  (write-line "")
  (write-line "Initialization options:")
  (write-line "")
  (write-line "--help\t\tdisplay this message and exit")
  (write-line "--version\t\tdisplay version information and exit")
  (exit 0))

(define (cli-version)
  (write-line "minara 0.1")
  (write-line "Copyright (C) 2004 Rob Myers")
  (write-line "You may redistribute copies of Emacs")
  (write-line "under the terms of the GNU General Public License.")
  (write-line 
   "For more information about these matters, see the file named COPYING.")
  (exit 0))

(define cli-option-spec
  '((version (single-char #\v))
    (help (single-char #\h))
    (file (single-char #\f) (value #t))))

;; Load the splash screen if required

(define (load-splash-screen)
  (make-window-from-file "../minara.minara"))

(define (cli-handle-arguments)
  (let* ((options (getopt-long (command-line) cli-option-spec))
	 (file-to-load (option-ref options 'file #f)))
    (if (option-ref options 'help #f)
	(cli-help))
    (if (option-ref options 'version #f)
	(cli-version))
    ;; Load the splash screen if no file, otherwise load file
    ;; Important, as GLUT crashes if started without a window!
    (if (equal? file-to-load #f)
	(load-splash-screen)
       	(make-window-from-file file-to-load))))
	 