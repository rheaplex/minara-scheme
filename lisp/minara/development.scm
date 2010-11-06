;; development.scm : loading and reloading tool and library files for minara
;;
;; Copyright (c) 2004 Rob Myers, rob@robmyers.org
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


(define-module (minara development)
  :use-module (minara tool)
  :use-module (minara keymap)
  :export ())


;; Reload libraries, tools and .minara file

;;(keymap-add-fun-global load-and-initialise "d" "l")

;; Reload tools

;;(define (reload-tool-files)
;;    (format #t "Reloading tool files~%")
;;  (remove-current-tool)
;;  (load-tools)
;;  (bind-event-hooks)
;;  (format #t "Reloaded tool files~%"))

;;(keymap-add-fun-global reload-tool-files "d" "t")

;; Reload .minara file

;;(define (reload-dot-minara-file)
;;    (format #t "Reloading .minara file~%")
;;  (remove-current-tool)
;;  (load-user-config)
;;  ;;(bind-event-hooks)
;;  (format #t "Reloaded .minara file~%"))

;;(keymap-add-fun-global reload-dot-minara-file "d" "d")

;; Edit minara file

;;(define (external-edit-current-window)
;;  (external-edit-file "~/.minara"))

;; Register keys for editing a window

;;(keymap-add-fun-global external-edit-current-window "x" "d")