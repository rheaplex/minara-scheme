;; picking-hit.scm : picking result record for minara
;;
;; Copyright (c) 2004-2006, 2009 Rob Myers, rob@robmyers.org
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
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (minara picking-hit)
  :use-module (srfi srfi-9)
  :export (make-picking-hit
           picking-hit?
           picking-hit-index
           picking-hit-from
           set-picking-hit-from!
           picking-hit-to
           set-picking-hit-to!
           picking-hit-transform))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A picking hit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type picking-hit
  (make-picking-hit index
                    from
                    to
                    transformation)
  picking-hit?
  ;; Which hit was it in the pick?
  (index picking-hit-index)
  ;; Which character in the buffer does the hit start at?
  (from picking-hit-from
        set-picking-hit-from!)
  ;; Which character in the buffer does the hit go to?
  (to picking-hit-to
      set-picking-hit-to!)
  ;; The transformation of the hit
  (transformation picking-hit-transform))
