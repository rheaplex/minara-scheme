;; development.scm : loading and reloading develoment tool and library files
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

;; Reload libraries, tools and .minara file

(keymap-add-fun %global-keymap 
		load-and-initialise
		"d" "I")

;; Reload tools

(define (reload-tool-files)
    (format #t "Reloading tool files~%")
  (remove-current-tool)
  (load-tools)
  (bind-event-hooks)
  (format #t "Reloaded tool files~%"))

(keymap-add-fun %global-keymap 
		reload-tool-files
		"d" "t")

;; Reload .minara file

(define (reload-dot-minara-file)
    (format #t "Reloading .minara file~%")
  (remove-current-tool)
  (load-user-config)
  ;;(bind-event-hooks)
  (format #t "Reloaded .minara file~%"))

(keymap-add-fun %global-keymap 
		reload-dot-minara-file
		"d" "d")
