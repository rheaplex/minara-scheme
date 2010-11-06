;; keymap.scm : keymaps for minara
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
;; Keymaps
;; Inspired by, but different from, Emacs keymaps.
;; In particular keymaps fit within more general event handlers, rather than
;; the other way round.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (minara keymap)
  :export (keymap-make
	   keymap-current-root
	   keymap?
	   keymap-current
	   reset-current-keymap
	   keymap-current-root-set
	   keymap-current-root-reset
	   keymap-current-set
	   keymap-add-fun-global
	   keymap-add-fun
	   keymap-add-fun-list
	   dispatch-keymap
	   dispatch-key
	   key-dispatch-hook-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window-system-specific constants
;; Here GLUT constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define GLUT_KEY_F1			1)
(define GLUT_KEY_F2			2)
(define GLUT_KEY_F3			3)
(define GLUT_KEY_F4			4)
(define GLUT_KEY_F5			5)
(define GLUT_KEY_F6			6)
(define GLUT_KEY_F7			7)
(define GLUT_KEY_F8			8)
(define GLUT_KEY_F9			9)
(define GLUT_KEY_F10			10)
(define GLUT_KEY_F11			11)
(define GLUT_KEY_F12			12)
;; directional keys
(define GLUT_KEY_LEFT			100)
(define GLUT_KEY_UP			101)
(define GLUT_KEY_RIGHT			102)
(define GLUT_KEY_DOWN			103)
(define GLUT_KEY_PAGE_UP		104)
(define GLUT_KEY_PAGE_DOWN		105)
(define GLUT_KEY_HOME			106)
(define GLUT_KEY_END			107)
(define GLUT_KEY_INSERT			108)
 
;; glutGetModifiers return mask.
(define GLUT_ACTIVE_SHIFT               1)
(define GLUT_ACTIVE_CTRL                2)
(define GLUT_ACTIVE_ALT                 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making and getting keymaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make an empty keymap alist
(define (keymap-make)
  (make-hash-table 31))

;; The one and only global keymap
(define %global-keymap (keymap-make))

;; The current root keymap
(define keymap-current-root #f)

;; Check that the object is a keymap
(define (keymap? keymap)
  (hash-table? keymap))

;; The current keymap
(define keymap-current keymap-current-root)

;; Reset the history
(define (reset-current-keymap)
  (set! keymap-current keymap-current-root))

;; Set the root keymap
(define (keymap-current-root-set keymap)
  (set! keymap-current-root keymap)
  (reset-current-keymap))

;; Reset the root keymap
(define (keymap-current-root-reset)
  (set! keymap-current-root #f))

;; Set the current keymap
(define (keymap-current-set keymap)
  (set! keymap-current keymap))

;; Put the key unto the global keymap
(define (keymap-add-fun-global fun . keys)
  (keymap-add-fun-list %global-keymap fun keys))

;; Add a possibly nested key fun to the keymap
;; A key is a basic key (aA1) prefixed with CA for control and/or alt

(define (keymap-add-fun keymap fun . keys)
  (keymap-add-fun-list keymap fun keys))

(define (keymap-add-fun-list keymap fun keys)
  (let* ((key (car keys))
         (rest-of-keys (cdr keys))
         (keymap-entry-for-key (hash-ref keymap
                                         key)))
    ;;(format #t "keys: ~a~%key: ~a~%keymap-for-key: ~a~%rest: ~a~%~%"
    ;;        keys key keymap-entry-for-key rest-of-keys)
    (cond
     ;; Last key? Insert in current keymap
     ((equal? rest-of-keys
              '())
      ;; Warn if rebinding key
      ;;(if (hash-ref keymap
      ;;              key)
      ;;    (format #t "Redefined key ~a in keymap ~a~%" key keymap))
      (hash-set! keymap
                 key
                 fun))
     ;; No keymap for key?
     ((equal? keymap-entry-for-key
             #f)
      ;; Create it
      (hash-set! keymap
                 key
                 (make-hash-table 31))
      ;; And recurse
      (keymap-add-fun-list (hash-ref keymap
                                     key)
                           fun
                           rest-of-keys))
     ;; Keymap exists but key is not last key?
     (else
      ;; If it's a key, replace witha  keymap
      (if (procedure? keymap-entry-for-key)
          (hash-set! keymap
                     key
                     (make-hash-table 31)))
      ;; Just recurse, re-getting keymap in case we replaced key with keymap
      (keymap-add-fun-list (hash-ref keymap
                                     key)
                           fun
                           rest-of-keys)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching keypresses through keymaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Try to dispatch the key press in the keymap
(define (dispatch-keymap keymap key)
  (let ((next-candidate (hash-ref keymap 
				  key)))
    (cond 
     ;; Keymap? Install as current keymap
     ((keymap? next-candidate)
      (keymap-current-set next-candidate)
      #t)
     ;; No match? Reset
     ((not next-candidate)
      (reset-current-keymap)
      #f)
     ;; Fun? Call
     (else ;;(fun? next-candidate)
      (next-candidate)
      (reset-current-keymap)
      #t))))

;; Try to dispatch the current keymap
(define (dispatch-key key)
    (if (not (and keymap-current 
		  (dispatch-keymap keymap-current 
				   key)))
	(if (not (dispatch-keymap %global-keymap 
				  key))
	    (format #t 
		    "No match for key ~a in current or global keymap.~%" 
		    key))))


;; Control really isn't happy on Fedora 13!
;; Limit it to a..z ONLY
;; Work out what's going wrong here and fix, or avoid modifiers altogether
(define (key-with-control-key key)
  (if (> (string-length key) 0)
      (let ((key-code (char->integer (string-ref key 0))))
	;; If it's a letter, make it an ascii letter, otherwise ignore
	(if (< key-code 27)
	    (string #\C (integer->char (+ key-code 96)))
	    #f))
      #f))

;; Our method to interface to the event system
;; Note that whilst getting shift alt ans control is GLUT-dependent,
;; once we make the booleans it could be any windowing system
(define (key-dispatch-hook-method win key modifiers)
  (let ((shift (= 1 (logand modifiers 
			    GLUT_ACTIVE_SHIFT)))
	(control (= 2 (logand modifiers 
			      GLUT_ACTIVE_CTRL)))
	(alt (= 4 (logand modifiers 
			  GLUT_ACTIVE_ALT))))
    ;; Shift is just the upper-case character. Ensure it *is* upper-case.
    (if control
	(set! key (key-with-control-key key)))
    (if (and key alt)
	(set! key (string-append "A" key)))
    ;; Like this because of the control-key problems, see key-with-control-key
    (if (and key shift)
	(set! key (string-upcase key)))
    (dispatch-key key)))


;; Install the cancel key into the global keymap
(keymap-add-fun %global-keymap reset-current-keymap "Cg")


;; TEST

; (define (dummy)
;   (write "hello"))

; (define (dummy2)
;   (write "hello 2"))

; (define km (keymap-make))

; (keymap-add-fun km dummy "a")
; (keymap-add-fun km dummy2 "b" "c")
; (keymap-current-root-set km)