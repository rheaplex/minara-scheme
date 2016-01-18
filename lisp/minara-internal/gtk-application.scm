;; gtk-application.scm : gtk application implementation for minara
;;
;; Copyright (c) 2016 Rob Myers, rob@robmyers.org
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
;; gtk-application
;; Windows and event loops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (minara-internal gtk-application)
  :use-module (cairo)
  :use-module (gnome-2)
  :use-module (gnome gtk)
  :use-module (gnome gtk gdk-event)
  :use-module (srfi srfi-11)
  :use-module (minara-internal cairo-rendering)
  :export (window-make
           window-display
           window-dispose
           window-current-id
           window-set
           window-set-title
           window-draw-text
           window-invalidate
           window-draw-begin
           window-draw-end
           event-callbacks-register
           event-loop-main))

;; Event callbacks

(define %quit-hook #f)
(define %resize-hook #f)
(define %draw-hook #f)
(define %mouse-down-hook #f)
(define %mouse-up-hook #f)
(define %mouse-move-hook #f)
(define %key-press-hook #f)
(define %key-release-hook #f)

;; Frontmost window
;; This needs much better integration with Gtk
(define %current-window #f)

(define %current-context #f)

(define (window-close-handler widget event)
  (%quit-hook)
  (gtk-main-quit)
  #t)

(define (resize-handler widget event)
  (let* ((a (get-allocation widget))
         (width (vector-ref a 2))
         (height (vector-ref a 3)))
    (%resize-hook (gtk-widget-get-toplevel widget) width height))
  #t)

(define (draw-handler widget event)
  (%draw-hook (gtk-widget-get-toplevel widget))
  #t)

(define (mouse-press-handler widget event)
  (let ((x (gdk-event-button:x event))
        (y (gdk-event-button:y event))
        (button (gdk-event-button:button event)))
    (%mouse-down-hook (gtk-widget-get-toplevel widget) button x y))
  #t)

(define (mouse-release-handler widget event)
  (let ((x (gdk-event-button:x event))
        (y (gdk-event-button:y event))
        (button (gdk-event-button:button event)))
    (%mouse-up-hook (gtk-widget-get-toplevel widget) button x y))
  #t)

(define (mouse-move-handler widget event)
  (let-values (((coords-ret x y) (gdk-event-get-coords event)))
    (%mouse-move-hook (gtk-widget-get-toplevel widget) x y))
  #t)

(define (expand-control-keys mods)
  (let ((mod-symbols '()))
    (if (memq 'shift-mask mods)
        (set! mod-symbols (cons 'shift mod-symbols)))
    (if (memq 'mod1-mask mods)
        (set! mod-symbols (cons 'alt mod-symbols)))
    (if (memq 'control-mask mods)
        (set! mod-symbols (cons 'control mod-symbols)))
    mod-symbols))

;; key-unicode: a ^           #f
;; key-name:    a asciicircum F5
;; So check for unicode. If no unicode use name. If name == mask, ignore
;;   Control_L/control-mask Control_R/control-mask Caps_Lock/lock-mask
;;   Shift_L/shift-mask Shift_R/shift-mask Alt_L/mod1-mask Alt_R/mod1-mask
;; mods: control-mask mod1-mask (modifier-mask) shift-mask

(define (key-press-handler widget event)
  (let* ((key-unicode (integer->char (gdk-keyval-to-unicode
                                      (gdk-event-key:keyval event))))
         (key-name (gdk-keyval-name (gdk-event-key:keyval event)))
         (mods (gdk-event-key:modifiers event)))
    ;; Filter control key presses
    (if (not (eq? key-unicode #\nul))
        (%key-press-hook (gtk-widget-get-toplevel widget)
                         key-unicode
                         (expand-control-keys mods))))
  #t)

(define (key-release-handler widget event)
  (let* ((key-unicode (integer->char (gdk-keyval-to-unicode
                                      (gdk-event-key:keyval event))))
         (key-name (gdk-keyval-name (gdk-event-key:keyval event)))
         (mods (gdk-event-key:modifiers event)))
    ;; Filter control key presses
    (if (not (eq? key-unicode #\nul))
        (%key-release-hook (gtk-widget-get-toplevel widget)
                           key-unicode
                           (expand-control-keys mods))))
  #t)

(define (window-make  width height)
  (let ((win (gtk-window-new 'toplevel))
        (drawing-area (gtk-drawing-area-new)))
    (gtk-widget-set-size-request drawing-area width height)
    (gtk-container-add win drawing-area)
    ;; Must be called before element is visible
    (set-events drawing-area '(button-press-mask
                               button-release-mask
                               pointer-motion-mask))
    ;; These must be on the window
    (connect win 'delete-event window-close-handler)
    (connect win 'key-press-event key-press-handler)
    (connect win 'key-release-event key-release-handler)
    ;; These must be on the drawing area
    (connect drawing-area 'configure-event resize-handler)
    (connect drawing-area 'expose-event draw-handler)
    (connect drawing-area 'motion-notify-event mouse-move-handler)
    (connect drawing-area 'button-press-event mouse-press-handler)
    (connect drawing-area 'button-release-event mouse-release-handler)
    win))

(define (window-display window)
  (window-set window)
  ;; We must do this after make-window calls window-make so window-for-id works
  (show-all window))

(define (window-dispose window)
  (gtk-widget-destroy window)
  ;; Should set to new front window if any
  (if (eq? window %current-window)
      (set! %current-window #f)))

(define (window-current-id)
  ;; Use the window as its own id
  %current-window)

(define (window-set window)
  (set! %current-window window))

(define (window-set-title window title)
  (gtk-window-set-title window title))

(define (window-draw-text window x y text)
  ;;De-hardcode all of me!
  (cairo-select-font-face %current-context
                          "sans-serif"
                          'normal
                          'bold)
  ;; Rather than call cairo-set-font-size, get the text the right way up
  (cairo-set-font-matrix %current-context (cairo-make-scale-matrix 13 -13))
  (cairo-set-source-rgb %current-context 0.9 0.8 0.8)
  (cairo-move-to %current-context (- x 1) (- y 1))
  (cairo-show-text %current-context text)
  (cairo-set-source-rgb %current-context 0.1 0.1 0.25)
  (cairo-move-to %current-context x y)
  (cairo-show-text %current-context text))

(define (window-invalidate window)
  (gtk-widget-queue-draw window))

(define (window-draw-begin window)
  (let* ((a (get-allocation window))
         (width (vector-ref a 2))
         (height (vector-ref a 3))
         (cr (gdk-cairo-create (window-drawing-area window))))
    (set! %current-context cr)
    (initialise-protocol %current-context width height)))

(define (window-draw-end window)
  (finalise-protocol)
  (cairo-destroy %current-context)
  (set! %current-context #f))

(define (event-callbacks-register quit-hook resize-hook draw-hook
                                  mouse-down-hook mouse-up-hook mouse-move-hook
                                  key-press-hook key-release-hook)
  (set! %quit-hook quit-hook)
  (set! %resize-hook resize-hook)
  (set! %draw-hook draw-hook)
  (set! %mouse-down-hook mouse-down-hook)
  (set! %mouse-up-hook mouse-up-hook)
  (set! %mouse-move-hook mouse-move-hook)
  (set! %key-press-hook key-press-hook)
  (set! %key-release-hook key-release-hook))

(define (event-loop-main)
  (gtk-main))

(define (window-drawing-area window)
  (gtk-widget-get-window
   (car (gtk-container-get-children
          (gtk-widget-get-toplevel window)))))
