;; menu.scm : menus for minara
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
;; Menus
;; We use GLUT contextual menus at the moment. This is terrible. Nothing should
;; be done that relies on this, or that would prevent proper pull-down menus
;; being used in future.
;;
;; Menu handlers receive the window id they're called on as their parameter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (minara menu)
  :use-module (minara events)
  :use-module (minara-internal events)
  :use-module (minara-internal menu)
  :export (menu-id-make
	   menu-callback-add
	   menu-callback-remove
	   menu-callback
	   menu-make-toplevel title
	   menu-make
	   menu-name
	   menu-id
	   menu-install-toplevel 
	   menu-make-entry
	   menu-delete-entry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The global menu / menu item id count 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define menu-item-id 1024)

(define (menu-id-make)
  (let ((id menu-item-id))
    (set! menu-item-id (+ menu-item-id 1))
    id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Callbacks for menu items
;; So when the window system passes us a "menu selected" event, we look up the
;; menu item id here and call the callback registered under that id.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define menu-callbacks '())

(define (menu-callback-add id callback)
  (set! menu-callbacks
	(acons id callback menu-callbacks)))

(define (menu-callback-remove id)
  (set! menu-callbacks
	(assq-remove! menu-callbacks id)))

(define (menu-callback id)
  (assoc id menu-callbacks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level menus
;; We only have 1-level deep menus at the moment, no nested submenus.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (menu-make-toplevel title)
  (list (menu-make) title))

(define (menu-name menu)
  (cadr menu))

(define (menu-id menu)
  (car menu))

(define (menu-install-toplevel menu)
  (menu-install (menu-id menu) (menu-name menu)))


;; Remove a menu
;; Unimplemented

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a menu item.
;; This installs a callback that will be called when this item is selected.

(define (menu-make-entry menu title callback)
  (let ((item-id (menu-id-make)))
    (menu-add-entry (menu-id menu) title item-id)
    (menu-callback-add item-id callback)
    item-id))

;; Remove a menu item

(define (menu-delete-entry menu-id)
  (menu-remove-entry menu-id)
  (menu-callback-remove menu-id))


;; Change a menu state

;; Get a menu state

;; Enable a menu item

;; Disble a menu item


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu event handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The standard menu event handler
;; Hooked into the main event system in events.scm
;; Don't replace.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (default-menu-handler window-id menu-id)
  (let ((handler (cdr (menu-callback menu-id))))
    (if handler
	(handler window-id))))
  
(define (install-default-menu-handler)
  (add-menu-select-hook default-menu-handler)
  (bind-event-hooks))

;; Just install the default menu handler to start with
(install-default-menu-handler)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (define (pen-dummy window-id)
;   (write "called pen dummy"))

; (define (colour-dummy window-id)
;   (write "called colour dummy"))

; (define (select-dummy window-id)
;   (write "called select dummy"))

; (define menoo (menu-make-toplevel "tools"))
; (menu-make-entry menoo "pen" pen-dummy)
; (menu-make-entry menoo "colour" colour-dummy)
; (menu-make-entry menoo "select" select-dummy)
; (menu-install-toplevel menoo)