;; events.scm : gui event handling for minara
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
;; Basic GUI and application events
;;
;; We receive events from the window system via the C code, which may have
;; done some setting up of the environment for us.
;;
;; We keep a list of handlers for each event, and call each in turn.
;;
;; People really *shouldn't* replace these functions, instead they should
;; use them to add and remove event hooks within the system set up here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (minara events)
  :use-module (system vm trace)
  :use-module (minara keymap)
  :use-module (minara window)
  :export (call-quit-hooks
           add-quit-hook
           remove-quit-hook
           call-resize-hooks
           add-resize-hook
           remove-resize-hook
           call-draw-hooks
           add-draw-hook
           remove-draw-hook
           call-mouse-down-hooks
           add-mouse-down-hook
           remove-mouse-down-hook
           call-mouse-up-hooks
           add-mouse-up-hook
           remove-mouse-up-hook
           call-mouse-move-hooks
           add-mouse-move-hook
           remove-mouse-move-hook
           call-key-press-hooks
           add-key-press-hook
           remove-key-press-hooks
           call-key-release-hooks
           add-key-release-hook
           remove-key-release-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quitting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %quit-funs '())

(define (call-quit-hooks)
  (for-each (lambda (fun)
              (fun))
            %quit-funs))

(define (add-quit-hook fun)
  (if (not (memq fun
                 %quit-funs))
      (set! %quit-funs
            (cons fun
                  %quit-funs))))

(define (remove-quit-hook fun)
  (set! %quit-funs
        (delq fun
              %quit-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resizing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %resize-funs '())

(define (call-resize-hooks win-id width height)
  (let ((win (window-for-id win-id)))
    (for-each (lambda (fun)
                (fun win
                     width
                     height))
              %resize-funs)))

(define (add-resize-hook fun)
  (if (not (memq fun
                 %resize-funs))
      (set! %resize-funs
            (cons fun
                  %resize-funs))))

(define (remove-resize-hook fun)
  (set! %resize-funs
        (delq fun
              %resize-funs)))

;; GLUT's window co-ords go down, OGL's go up.
;; So we need to allow for this

(define (%update-window-dimensions win width height)
  (set-window-width! win width)
  (set-window-height! win height))

(add-resize-hook %update-window-dimensions)

(define (%swizzle-y win y)
  (- (window-height win)
     y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %draw-funs '())

(define (call-draw-hooks win-id)
  (let ((win (window-for-id win-id)))
    (for-each (lambda (fun)
                (fun win))
              %draw-funs)))

(define (add-draw-hook fun)
  (if (not (memq fun
                 %draw-funs))
      (set! %draw-funs
            (cons fun
                  %draw-funs))))

(define (remove-draw-hook fun)
  (set! %draw-funs
        (delq fun
              %draw-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse down
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %mouse-down-funs '())

(define (call-mouse-down-hooks win-id button x y)
  (let ((win (window-for-id win-id)))
    (let ((yy (%swizzle-y win
                          y)))
      (for-each (lambda (fun)
                  (fun win
                       button
                       x
                       yy))
                %mouse-down-funs))))

(define (add-mouse-down-hook fun)
  (if (not (memq fun
                 %mouse-down-funs))
      (set! %mouse-down-funs
            (cons fun
                  %mouse-down-funs))))

(define (remove-mouse-down-hook fun)
  (set! %mouse-down-funs
        (delq fun
              %mouse-down-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %mouse-up-funs '())

(define (call-mouse-up-hooks win-id button x y)
  (let ((win (window-for-id win-id)))
    (let ((yy (%swizzle-y win
                          y)))
      (for-each (lambda (fun)
                  (fun win
                       button
                       x
                       yy))
                %mouse-up-funs))))

(define (add-mouse-up-hook fun)
  (if (not (memq fun
                 %mouse-up-funs))
      (set! %mouse-up-funs
            (cons fun
                  %mouse-up-funs))))

(define (remove-mouse-up-hook fun)
  (set! %mouse-up-funs
        (delq fun
              %mouse-up-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %mouse-move-funs '())

(define (call-mouse-move-hooks win-id x y)
  (let ((win (window-for-id win-id)))
    (let ((yy (%swizzle-y win
                          y)))
      (for-each (lambda (fun)
                  (fun win
                       x
                       yy))
                %mouse-move-funs))))

(define (add-mouse-move-hook fun)
  (if (not (memq fun
                 %mouse-move-funs))
      (set! %mouse-move-funs
            (cons fun
                  %mouse-move-funs))))

(define (remove-mouse-move-hook fun)
  (set! %mouse-move-funs
        (delq fun
              %mouse-move-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key presses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %key-press-funs '())

(define (call-key-press-hooks win-id key modifiers)
  (let ((win (window-for-id win-id)))
    (for-each (lambda (fun)
                (fun win
                     key
                     modifiers))
              %key-press-funs)))

(define (add-key-press-hook fun)
  (if (not (memq fun
                 %key-press-funs))
      (set! %key-press-funs
            (cons fun
                  %key-press-funs))))

(define (remove-key-press-hook fun)
  (set! %key-press-funs
        (delq fun
              %key-press-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key releases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %key-release-funs '())

(define (call-key-release-hooks win-id key modifiers)
  (let ((win (window-for-id win-id)))
    (for-each (lambda (fun)
                (fun win
                     key
                     modifiers))
              %key-release-funs)))

(define (add-key-release-hook fun)
  (if (not (memq fun
                 %key-release-funs))
      (set! %key-release-funs
            (cons fun
                  %key-release-funs))))

(define (remove-key-release-hook fun)
  (set! %key-release-funs
        (delq fun
              %key-release-funs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events from built-in modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hook into the event system
(add-key-release-hook key-dispatch-hook-method)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-draw-hook window-redraw-event)

;; Register keys for editing a window

(keymap-add-fun-global external-edit-current-window "x" "e")
