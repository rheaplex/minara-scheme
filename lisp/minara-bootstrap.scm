;; minara-bootstrap.scm : main setup code for minara
;;
;; Copyright (c) 2004, 2010, 2016 Rob Myers, rob@robmyers.org
;;
;; This file is part of minara.
;;
;; minara is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; minara is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main startup code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(read-enable 'positions)


(define-module (minara-bootstrap)
  :use-module (minara config)
  :use-module (minara events)
  :use-module (minara command-line)
  :use-module (minara selection)
  :use-module (minara-internal gtk-application))

(define library-defs
  '(("tools" . ("colour-tools"
                "pen-tools"
                "shape-tools"))))

(define (load-file path file)
  (let ((filepath (string-append path "/" file ".scm")))
    (display "Loading: ")(display filepath)(newline)
    (primitive-load filepath)))

(define (load-files path files)
  (for-each (lambda (file)
              (load-file path file))
            files))

(for-each (lambda (library-def)
            (let ((dir (car library-def))
                  (files (cdr library-def)))
              (load-files (string-append $minara-lisp-dir "/" dir)
                          files)))
          library-defs)

(event-callbacks-register call-quit-hooks
                          call-resize-hooks
                          call-draw-hooks
                          call-mouse-down-hooks
                          call-mouse-up-hooks
                          call-mouse-move-hooks
                          call-key-press-hooks
                          call-key-release-hooks)

(cli-handle-arguments)

(event-loop-main)
