;; rendering.scm : rendering hooks
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
;; The Rendering (and Picking!) Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rendering
(use-modules ((rendering)
              :renamer (symbol-prefix-proc 'rendering:)))

;; The top-level bindings for the protocol functions

(define write-header #f)
(define write-footer #f)
(define set-colour #f)
(define path-begin #f)
(define path-end #f)
(define move-to #f)
(define line-to #f)
(define curve-to #f)

;; Install the window rendering protocol

(define (install-window-rendering-protocol)
  (set! set-colour rendering:set-colour)
  (set! path-begin rendering:path-begin)
  (set! path-end rendering:path-end)
  (set! move-to rendering:move-to)
  (set! line-to rendering:line-to)
  (set! curve-to rendering:curve-to))


;; Macro to save/restore current protocol