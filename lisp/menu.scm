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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Menu handlers receive the window id they're called on as their parameter.

;; The global menu / menu item id count 

(define menu-item-id 1024)

(define (menu-id-make)
  (let ((id menu-item-id))
    (set! menu-item-id (+ menu-item-id 1))
    id))

;; Callbacks for menu items

(define menu-callbacks '())

(define (menu-callback-add id callback)
  (set! menu-callbacks
	(acons id callback menu-callbacks)))

(define (menu-callback-remove id)
  (set! menu-callbacks
	(assq-remove! menu-callbacks id)))

(define (menu-callback id)
  (assoc id menu-callbacks))

;; Create a (top-level) menu
;; We only have 1-level deep menus at the moment

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

;; Create a menu item, installing a callback

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

;; The standard menu event handler

(define (default-menu-handler window-id menu-id)
  (let ((handler (cdr (menu-callback menu-id))))
    (if handler
	(handler window-id))))
  
(define (install-default-menu-handler)
  (set! %menu-select-hook default-menu-handler)
  (bind-event-hooks))

;; Just install the default menu handler to start with
(install-default-menu-handler)



;; testing

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