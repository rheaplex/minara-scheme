;; tool.scm : tool handling for minara
;;
;; Copyright (c) 2004, 2010 Rob Myers, rob@robmyers.org
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
;; Tools
;; A tool is a set of event handlers and routines to install/uninstall them,
;; hooked up to the main keymap and the tool menu.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (minara tool)
  :use-module (minara window)
  :use-module (minara menu)
  :use-module (minara keymap)
  :export (install-tool
           remove-current-tool
           set-current-tool-name!
           current-tool-name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install a tool
;; This gives access to the tool from the menu and keyboard,
;; And installs an uninstall function (setup can be done by install-fun)
;; The install has to add the event handlers, the uninstall has to remove them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-tool install-fun uninstall-fun menu-name . key-combo)
  (let ((install-fun-with-boilerplate
         (lambda ()
           (remove-current-tool)
           (set-current-tool-name! menu-name)
           (set! %remove-current-tool-hook uninstall-fun)
           (install-fun))))
    (menu-callback-add menu-name install-fun-with-boilerplate)
    (apply keymap-add-fun-global
           install-fun-with-boilerplate
           key-combo)))

;; Handle a tool stopping being the current tool

(define (%remove-current-tool-hook)
  #f)

;; Keep track of the current tool name

(define %current-tool-name "")

(define (current-tool-name)
  %current-tool-name)

(define (set-current-tool-name! name)
  ;;  (set! %current-tool-name name)
  (set-window-tool-name! (window-current) name)
  (window-redraw (window-current)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove the current tool
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-current-tool)
  (if %remove-current-tool-hook
      (%remove-current-tool-hook))
  (set! %remove-current-tool-hook #f)
  (set-current-tool-name! "")
  (set-window-tool-name! (window-current) "")
  (window-redraw (window-current)))
