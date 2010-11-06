;;; gap-buffer.scm --- string buffer that supports point

;;	Copyright (C) 2002,2003,2004,2005 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; As a special exception, the Free Software Foundation gives permission
;; for additional uses of the text contained in its release of GUILE.
;;
;; The exception is that, if you link the GUILE library with other files
;; to produce an executable, this does not by itself cause the
;; resulting executable to be covered by the GNU General Public License.
;; Your use of that executable is in no way restricted on account of
;; linking the GUILE library code into it.
;;
;; This exception does not however invalidate any other reasons why
;; the executable file might be covered by the GNU General Public License.
;;
;; This exception applies only to the code released by the
;; Free Software Foundation under the name GUILE.  If you copy
;; code from other Free Software Foundation releases into a copy of
;; GUILE, as the General Public License permits, the exception does
;; not apply to the code that you add in this way.  To avoid misleading
;; anyone as to the status of such modified files, you must delete
;; this exception notice from them.
;;
;; If you write modifications of your own for GUILE, it is your choice
;; whether to permit this exception to apply to your modifications.
;; If you do not wish that, delete this exception notice.

;;; Author: Thien-Thi Nguyen <ttn@gnu.org>

;;; Commentary:

;; A gap buffer is a structure that models a string but allows relatively
;; efficient insertion of text somewhere in the middle.  The insertion
;; location is called `point' with minimum value 1, and a maximum value the
;; length of the string (which is not fixed).
;;
;; Specifically, we allocate a continuous buffer of characters that is
;; composed of the BEFORE, the GAP and the AFTER (reading L->R), like so:
;;
;;                          +--- POINT
;;                          v
;;    +--------------------+--------------------+--------------------+
;;    |       BEFORE       |        GAP         |       AFTER        |
;;    +--------------------+--------------------+--------------------+
;;
;;     <----- bef-sz ----->|<----- gap-sz ----->|<----- aft-sz ----->
;;
;;     <-------------------|       usr-sz       |------------------->
;;
;;     <-------------------------- all-sz -------------------------->
;;
;; This diagram also shows how the different sizes are computed, and the
;; location of POINT.  Note that the user-visible buffer size `usr-sz' does
;; NOT include the GAP, while the allocation `all-sz' DOES.
;;
;; The consequence of this arrangement is that "moving point" is simply a
;; matter of kicking characters across the GAP, while insertion can be viewed
;; as filling up the gap, increasing `bef-sz' and decreasing `gap-sz'.  When
;; `gap-sz' falls below some threshold, we reallocate with a larger `all-sz'.
;;
;; In the implementation, we actually keep track of the AFTER start offset
;; `aft-ofs' since it is used more often than `gap-sz'.  In fact, most of the
;; variables in the diagram are for conceptualization only.
;;
;; A gap buffer port is a "soft port" that wraps a gap buffer.
;;
;; (The term and concept of "gap buffer" are borrowed from Emacs.  We will
;; gladly return them when libemacs.so is available. ;-)
;;
;; A gap-buffer object has the following printed representation:
;;
;; #<gap-buffer GAP-SIZE/ALLOC-SIZE:POINT-MIN:POINT:POINT-MAX>
;;
;; with all fields (GAP-SIZE, ALLOC-SIZE, MINT-MIN, POINT, and POINT-MAX)
;; integers, and everything else as shown here.

;;; Code:

;;(define-module (ice-9 gap-buffer)
(define-module (minara gap-buffer)
  #:autoload (ice-9 rw) (write-string/partial)
  #:autoload (ice-9 receive) (receive)
  #:autoload (ice-9 regex) (string-match match:end)
  #:export (gb?
            make-gap-buffer
            gb-toggle-read-only
            gb-point
            gb-point-min
            gb-point-max
            gb-bolp gb-eolp gb-bobp gb-eobp
            gb-insert-string!
            gb-insert-char!
            gb-insert
            gb-delete-char!
            gb-delete-region
            gb-erase!
            gb-goto-char
            gb-forward-char gb-backward-char
            gb-forward-line gb-beginning-of-line gb-end-of-line
            gb-match-string
            gb-looking-at
            gb-match-beginning gb-match-end
            gb-search-forward gb-search-backward
            gb-re-search-forward
            gb-replace-match
            gb->port!
            gb->string
            gb->substring
            gb-filter!
            gb->lines
            gb-filter-lines!
            make-gap-buffer-port))

(define gap-buffer
  (make-record-type 'gap-buffer
                    '(s                 ; the buffer, a string
                      gap-ofs           ; GAP starts, aka (1- point)
                      aft-ofs           ; AFTER starts
                      ro)               ; read-only; insert/delete => error
                    (lambda (obj port)
                      (display "#<gap-buffer " port)
                      (display (- (aft-ofs: obj) (gap-ofs: obj)) port)
                      (display #\/ port)
                      (display (string-length (s: obj)) port)
                      (display #\: port)
                      (display (gb-point-min obj) port)
                      (display #\: port)
                      (display (gb-point obj) port)
                      (display #\: port)
                      (display (gb-point-max obj) port)
                      (display ">" port))))

;; Return #t iff @var{object} is a gap buffer object.
;;
(define (gb? object)
  ((record-predicate gap-buffer) object))

(define s:       (record-accessor gap-buffer 's))
(define gap-ofs: (record-accessor gap-buffer 'gap-ofs))
(define aft-ofs: (record-accessor gap-buffer 'aft-ofs))
(define ro:      (record-accessor gap-buffer 'ro))

(define s!       (record-modifier gap-buffer 's))
(define gap-ofs! (record-modifier gap-buffer 'gap-ofs))
(define aft-ofs! (record-modifier gap-buffer 'aft-ofs))
(define ro!      (record-modifier gap-buffer 'ro))

(define (check-read-only gb)
  (and (ro: gb) (error "buffer read-only:" gb)))

;; todo: expose
(define default-chunk-size 1024)
(define default-realloc-threshold 32)

(define (round-up n)
  (* default-chunk-size (+ 1 (quotient n default-chunk-size))))

(define new (record-constructor gap-buffer))

(define (realloc gb inc)
  (let* ((old-s   (s: gb))
         (all-sz  (string-length old-s))
         (new-sz  (+ all-sz inc))
         (gap-ofs (gap-ofs: gb))
         (aft-ofs (aft-ofs: gb))
         (new-s   (make-string new-sz))
         (new-aft-ofs (+ aft-ofs inc)))
    (substring-move! old-s 0 gap-ofs new-s 0)
    (substring-move! old-s aft-ofs all-sz new-s new-aft-ofs)
    (s! gb new-s)
    (aft-ofs! gb new-aft-ofs)))

;; Return a new gap buffer.  Optional arg @var{init} is either a port
;; to read from, or a string, used to initialize the buffer contents.
;; Point is left at the maximum position.
;;
;;-sig: ([init])
;;
(define (make-gap-buffer . init)
  (define (string<- x)
    (cond ((string? x) x)
          ((port? x) (let ((ls (list #f))
                           (next (lambda () (read-char x))))
                       (let loop ((c (next)) (tp ls))
                         (if (eof-object? c)
                             (list->string (cdr ls))
                             (begin (set-cdr! tp (list c))
                                    (loop (next) (cdr tp)))))))
          (else (error "bad init type"))))
  (let* ((string (if (null? init)
                     ""
                     (string<- (car init))))
         (len (string-length string))
         (alloc (round-up len))
         (s (make-string alloc)))
    (substring-move! string 0 len s 0)
    (new s len alloc #f)))

;; Change whether @var{gb} is read-only.
;; With arg, set read-only iff arg is positive.
;;
;;-sig: (gb [arg])
;;
(define (gb-toggle-read-only gb . arg)
  (let* ((cur (ro: gb))
         (now (cond ((null? arg) (not cur))
                    ((car arg) => (lambda (v)
                                    (and v (number? v) (positive? v)))))))
    ;; compact on transition from rw to ro
    (and (not cur) now
         (let ((gap (gap-ofs: gb))
               (aft (aft-ofs: gb)))
           (or (= gap aft)
               ;; present approach: realloc (make new string);
               ;; todo: set high-water mark and avoid realloc (re-use string)
               (let* ((s   (s: gb))
                      (len (string-length s))
                      (new (make-string (+ gap (- len aft)))))
                 (substring-move! s 0 gap new 0)
                 (substring-move! s aft len new gap)
                 (s! gb new)
                 (aft-ofs! gb gap)))))
    (ro! gb now))
  (if #f #f))

;; Return the position of point in @var{gb}.
;; This is an integer starting with 1 (one).
;;
(define (gb-point gb)
  (1+ (gap-ofs: gb)))

;; Return the minimum position possible for point in @var{gb}.
;; At this time, this value is always 1 (one).
;;
(define (gb-point-min gb) 1)            ; no narrowing (for now)

;; Return the maximum position possible for point in @var{gb}.
;; This value can be changed by inserting text into the buffer,
;; and is limited by Guile's string implementation.
;;
(define (gb-point-max gb)
  (1+ (- (string-length (s: gb)) (- (aft-ofs: gb) (gap-ofs: gb)))))

(define (nl? s i)
  (char=? #\newline (string-ref s i)))

;; Return #t if point in @var{gb} is at the beginning of a line.
;;
(define (gb-bolp gb)
  (or (= (gb-point gb) (gb-point-min gb))
      (nl? (s: gb) (1- (gap-ofs: gb)))))

;; Return #t if point in @var{gb} is at the end of a line.
;;
(define (gb-eolp gb)
  (or (= (gb-point gb) (gb-point-max gb))
      (nl? (s: gb) (aft-ofs: gb))))

;; Return #t if point is at the beginning of @var{gb}.
;;
(define (gb-bobp gb)
  (= 1 (gb-point gb)))

;; Return #t if point is at the end of @var{gb}.
;;
(define (gb-eobp gb)
  (= (gb-point-max gb) (gb-point gb)))

(define (insert-prep gb len)
  (let* ((gap-ofs (gap-ofs: gb))
         (aft-ofs (aft-ofs: gb))
         (slack (- (- aft-ofs gap-ofs) len)))
    (and (< slack default-realloc-threshold)
         (realloc gb (round-up (- slack))))
    gap-ofs))

;; Insert into @var{gb} a @var{string}, moving point forward as well as
;; increasing the value that would be returned by @code{gb-point-max}.
;;
(define (gb-insert-string! gb string)
  (check-read-only gb)
  (let* ((len (string-length string))
         (gap-ofs (insert-prep gb len)))
    (substring-move! string 0 len (s: gb) gap-ofs)
    (gap-ofs! gb (+ gap-ofs len))))

;; Insert into @var{gb} a single @var{char}, moving point forward as well as
;; increasing the value that would be returned by @code{gb-point-max}.
;;
(define (gb-insert-char! gb char)
  (check-read-only gb)
  (let ((gap-ofs (insert-prep gb 1)))
    (string-set! (s: gb) gap-ofs char)
    (gap-ofs! gb (+ gap-ofs 1))))

;; Insert the arguments at point.
;; If an arg is a gap-buffer, insert its contents.
;; If an arg is a pair, insert a string made by applying @code{write} to it.
;; If an arg is a number, insert the result of @code{number->string}.
;; Other types accepted: char, string, symbol.
;; Point moves forward to end up after the inserted text.
;;
(define (gb-insert gb . args)
  (let ((ins! (lambda (s) (gb-insert-string! gb s))))
    (for-each (lambda (x)
                (cond ((gb? x) (ins! (gb->string x)))
                      ((char? x) (gb-insert-char! gb x))
                      ((string? x) (ins! x))
                      ((symbol? x) (ins! (symbol->string x)))
                      ((number? x) (ins! (number->string x)))
                      ((pair? x) (ins! (with-output-to-string
                                         (lambda () (write x)))))
                      (else (error "wrong type arg"))))
              args)))

;; In @var{gb}, delete @var{count} characters from point, forward if
;; @var{count} is positive, backward if @var{count} is negative.  (If
;; @var{count} is zero, do nothing.)  Deleting backwards moves point
;; backwards.  Deleting forwards or backwards decreases the value that would
;; be returned by @code{gb-point-max}.
;;
(define (gb-delete-char! gb count)
  (check-read-only gb)
  (cond ((< count 0)                    ; backwards
         (gap-ofs! gb (max 0 (+ (gap-ofs: gb) count))))
        ((> count 0)                    ; forwards
         (aft-ofs! gb (min (string-length (s: gb)) (+ (aft-ofs: gb) count))))
        ((= count 0)                    ; do nothing
         #t)))

;; Delete text between @var{beg} and @var{end}.
;;
(define (gb-delete-region gb beg end)
  (check-read-only gb)
  (let ((b (min beg end))
        (e (max beg end)))
    (gb-goto-char gb b)
    (gb-delete-char! gb (- e b))))

;; Completely erase @var{gb}.  Point is left at the minimum position possible
;; (which happens to be also the maximum position possible since the buffer
;; is empty).
;;
(define (gb-erase! gb)
  (check-read-only gb)
  (gap-ofs! gb 0)
  (aft-ofs! gb (string-length (s: gb))))

(define (point++n! gb n s gap-ofs aft-ofs) ; n>0; warning: reckless
  (substring-move-left! s aft-ofs (+ aft-ofs n) s gap-ofs)
  (gap-ofs! gb (+ gap-ofs n))
  (aft-ofs! gb (+ aft-ofs n)))

(define (point+-n! gb n s gap-ofs aft-ofs) ; n<0; warning: reckless
  (substring-move-right! s (+ gap-ofs n) gap-ofs s (+ aft-ofs n))
  (gap-ofs! gb (+ gap-ofs n))
  (aft-ofs! gb (+ aft-ofs n)))

;; In @var{gb}, move point to @var{new-point} and return it.  If
;; @var{new-point} is outside the minimum and maximum positions possible, it
;; is adjusted to the the nearest boundary (however, the return value is
;; @var{new-point} unchanged).
;;
(define (gb-goto-char gb new-point)
  (let ((pmax (gb-point-max gb)))
    (or (and (< new-point 1)    (gb-goto-char gb 1))
        (and (> new-point pmax) (gb-goto-char gb pmax))
        (let ((delta (- new-point (gb-point gb))))
          (or (= delta 0)
              ((if (< delta 0)
                   point+-n!
                   point++n!)
               gb delta (s: gb) (gap-ofs: gb) (aft-ofs: gb))))))
  new-point)

;; In gap-buffer @var{gb}, move point forward @var{n} characters.
;;
(define (gb-forward-char gb n)
  (or (= 0 n)
      ((if (> n 0) point++n! point+-n!)
       gb n (s: gb) (gap-ofs: gb) (aft-ofs: gb))))

;; In gap-buffer @var{gb}, move point backward @var{n} characters.
;;
(define (gb-backward-char gb n)
  (gb-forward-char gb (- n)))

(define (forward-line-internal gb n)    ; => (REMAINING . AT-LEAST-ONE?)
  (let ((s (s: gb))
        (gap (gap-ofs: gb))
        (aft (aft-ofs: gb)))
    (if (positive? n)
        (let loop ((n n) (start aft) (at-least-one? #f))
          (cond ((= 0 n)
                 (point++n! gb (- start aft) s gap aft)
                 (cons 0 at-least-one?))
                ((string-index s #\newline start)
                 => (lambda (nl)
                      (loop (1- n) (1+ nl) #t)))
                (else
                 (cons
                  (let ((pmax (gb-point-max gb)))
                    ;; "With positive N, a non-empty line at the end counts as
                    ;;  one line successfully moved (for the return value)."
                    (cond ((= pmax (gb-point gb)) n)
                          (else (gb-goto-char gb pmax)
                                (if at-least-one?
                                    n
                                    (1- n)))))
                  at-least-one?))))
        (let loop ((n n) (end gap))
          (cond ((= 1 n)
                 (point+-n! gb (- (1+ end) gap) s gap aft)
                 (cons 0 #f))
                ((string-rindex s #\newline 0 end)
                 => (lambda (nl)
                      (loop (1+ n) nl)))
                (else
                 (gb-goto-char gb (gb-point-min gb))
                 (cons n #f)))))))

;; In gap-buffer @var{gb}, move point @var{n} lines forward (backward if
;; @var{n} is negative).  Precisely, if point is on line @code{I}, move to the
;; start of line @code{I + N}.  If there isn't room, go as far as possible (no
;; error).  Return the count of lines left to move.  If moving forward, that
;; is @var{n} - number of lines moved; if backward, @var{n} + number moved.
;; With positive @var{n}, a non-empty line at the end counts as one line
;; successfully moved (for the return value).
;;
(define (gb-forward-line gb . n)
  (car (forward-line-internal gb (if (null? n) 1 (car n)))))

;; In gap-buffer @var{gb}, move point to beginning of current line.
;; With argument @var{n} not #f or 1, move forward @var{n} - 1 lines first.
;; If point reaches the beginning or end of buffer, it stops there.
;;
(define (gb-beginning-of-line gb . n)
  (forward-line-internal gb (1- (if (null? n) 1 (car n))))
  #f)

;; In gap-buffer @var{gb}, move point to end of current line.
;; With argument @var{n} not #f or 1, move forward @var{n} - 1 lines first.
;; If point reaches the beginning or end of buffer, it stops there.
;;
(define (gb-end-of-line gb . n)
  (let* ((n (if (null? n) 1 (car n)))
         (p-before (gb-point gb))
         (move-info (forward-line-internal gb n))
         (at-least-one? (cdr move-info))
         (p (gb-point gb)))
    (or (if (positive? n)
            (and (= p (gb-point-max gb))
                 (if at-least-one?
                     (< 1 n)
                     (let ((s (s: gb))
                           (gap (gap-ofs: gb)))
                       (or (= 1 n)
                           (not (string-rindex s #\newline
                                               (- gap (- p p-before))
                                               gap))))))
            (= p (gb-point-min gb)))
        (gb-forward-char gb -1)))
  #f)

;;; search (and replace)

;; "life is a search for truth; there is no truth." -- the ancients
;;
;; match data form
;;  emacs               guile                           (ice-9 gap-buffer)
;;  (BEG-0 END-0 ...)   #(STRING (BEG-0 . END-0) ...)   ((BEG-0 . END-0) ...)

(define (guile-md->md guile-md ofs)
  (map (lambda (cell)
         (cons (+ ofs (car cell))
               (+ ofs (cdr cell))))
       (cdr (vector->list guile-md))))

(define --match-data (make-object-property))

(define (md  gb)         (--match-data gb))
(define (md! gb m) (set! (--match-data gb) m))

;; Return string of text matched by last search.
;; @var{n} specifies which parenthesized expression in the last regexp.
;; Value is #f if @var{n}th pair didn't match, or there were less than
;; @var{n} pairs.  Zero means the entire text matched by the whole regexp
;; or whole string.
;;
;;-note: arg `string' purposefully left undocumented. --ttn
;;-sig: (gb n)
;;
(define (gb-match-string gb n . string)
  (let ((mn (list-ref (md gb) n)))
    (if (null? string)
        (bsub gb (1- (car mn)) (1- (cdr mn)))
        (substring (car string) (car mn) (cdr mn)))))

;; Return #t if text after point matches regular expression @var{re-str}.
;; This function modifies the match data that @code{gb-match-beginning},
;; @code{gb-match-end} and @code{gb-match-data} access; save and restore
;; the match data if you want to preserve them.
;;
(define (gb-looking-at gb re-str)
  (let* ((s (s: gb))
         (start (aft-ofs: gb))
         (gm (if (char=? #\^ (string-ref re-str 0))
                 (string-match re-str s start regexp/newline)
                 (string-match (string-append "^" re-str) s start))))
    (cond ((and gm (= start (car (vector-ref gm 1))))
           (md! gb (guile-md->md gm (1+ (- (gap-ofs: gb) start))))
           #t)
          (else #f))))

;; Return position of start of text matched by last search.
;; @var{subexp}, a number, specifies which parenthesized expression
;; in the last regexp.  Value is #f if @var{subexp}th pair didn't match,
;; or there were less than @var{subexp} pairs.  Zero means the entire text
;; matched by the whole regexp.
;;
;;-sig: ([n])
;;
(define (gb-match-beginning gb . n)
  (car (list-ref (md gb) (if (null? n) 0 (car n)))))

;; Return position of end of text matched by last search.
;; @var{subexp}, a number, specifies which parenthesized expression in the
;; last regexp.  Value is nil if @var{subexp}th pair didn't match, or there
;; were less than @var{subexp} pairs.  Zero means the entire text matched by
;; the whole regexp.
;;
;;-sig: ([n])
;;
(define (gb-match-end gb . n)
  (cdr (list-ref (md gb) (if (null? n) 0 (car n)))))

(define (parse-search-args gb edge args what)
  (let* ((bound (if (or (null? args)
                        (eq? #f (car args)))
                    edge
                    (car args)))
         (fail (cond ((or (< (length args) 2) (eq? #f (cadr args)))
                      (lambda () (error "search failed:" what)))
                     ((eq? #t (cadr args))
                      (lambda () #f))
                     (else
                      (lambda () (gb-goto-char gb bound) #f))))
         (reps (if (< (length args) 3) 1 (caddr args))))
    (values bound fail reps)))

;; Search forward from point for @var{string}.
;; Set point to the end of the occurrence found, and return point.
;; An optional second argument bounds the search; it is a buffer position.
;; The match found must not extend after that position.  #f is equivalent
;;   to (point-max).
;; Optional third argument, if #t, means if fail just return #f (no error).
;;   If not #f and not #t, move to limit of search and return #f.
;; Optional fourth argument is repeat count--search for successive occurrences.
;;
;;-todo: use string-index more efficiently
;;-sig: (string [bound [noerror [count]]])
;;
(define (gb-search-forward gb string . args)
  (let* ((p (gb-point gb))
         (pmax (gb-point-max gb)))
    (receive (bound fail reps) (parse-search-args gb pmax args string)
      (if (string=? "" string)
          p
          (let* ((len (string-length string))
                 (len-1 (1- len))
                 (s (s: gb))
                 (start (aft-ofs: gb))
                 (fc (string-ref string 0))               ; first char
                 (lc (string-ref string (1- len)))        ; last char
                 (next (lambda (from)
                         (string-index s fc from))))
            (let loop ((alook (next start)))
              (let ((look (and alook (+ p (- alook start)))))
                (cond
                 ;; no match / done
                 ((or (not look)
                      (not (< look bound))
                      (not (< (+ look len-1) bound)))
                  (fail))
                 ;; match / done if `reps' satisfied otherwise keep going
                 ((and (char=? (string-ref s (+ alook len-1)) lc)
                       (string=? (substring s alook (+ alook len)) string))
                  (set! reps (1- reps))
                  (if (= 0 reps)
                      (let ((rest-point (+ look len)))
                        (md! gb `((,look . ,rest-point)))
                        (gb-goto-char gb rest-point)
                        rest-point)     ; retval
                      (loop (next (+ alook len)))))
                 ;; no match / keep going
                 (else (loop (next (1+ alook))))))))))))

;; Search backward from point for @var{string}.
;; Set point to the beginning of the occurrence found, and return point.
;; An optional second argument bounds the search; it is a buffer position.
;; The match found must not extend before that position.
;; Optional third argument, if t, means if fail just return nil (no error).
;;  If not nil and not t, position at limit of search and return nil.
;; Optional fourth argument is repeat count--search for successive occurrences.
;;
;;-sig: (string [bound [noerror [repeat]]])
;;
(define (gb-search-backward gb string . args)
  (let* ((p (gb-point gb))
         (pmin (gb-point-min gb)))
    (receive (bound fail reps) (parse-search-args gb pmin args string)
      (if (string=? "" string)
          p
          (let* ((len (string-length string))
                 (len-1 (1- len))
                 (s (s: gb))
                 (fc (string-ref string 0))               ; first char
                 (lc (string-ref string (1- len)))        ; last char
                 (next (lambda (to)
                         (string-rindex s lc 0 to))))
            (let loop ((blook (next (gap-ofs: gb))))
              (let ((look (and blook (+ pmin blook))))
                (cond
                 ;; no match / done
                 ((or (not look)
                      (not (>= look bound))
                      (not (>= (- look len-1) bound)))
                  (fail))
                 ;; match / done if `reps' satisfied otherwise keep going
                 ((and (char=? (string-ref s (- blook len-1)) fc)
                       (string=? (substring s (- blook len-1) (1+ blook))
                                 string))
                  (set! reps (1- reps))
                  (if (= 0 reps)
                      (let ((rest-point (1+ (- look len))))
                        (md! gb `((,rest-point . ,(1+ look))))
                        (gb-goto-char gb rest-point)
                        rest-point)     ; retval
                      (loop (next (- blook len)))))
                 ;; no match / keep going
                 (else (loop (next (1- blook))))))))))))

;; Search forward from point for regular expression @var{regexp}.
;; Set point to the end of the occurrence found, and return point.
;; An optional second argument bounds the search; it is a buffer position.
;; The match found must not extend after that position.
;; Optional third argument, if #t, means if fail just return #f (no error).
;;   If not #f and not #t, move to limit of search and return #f.
;; Optional fourth argument is repeat count--search for successive occurrences.
;;
;; @var{regexp} may be a string, or compiled regular expression made with
;; @code{make-regexp}, in which case, it is the caller's decision whether or
;; not to include the flag @code{regexp/newline} (normally used when
;; @var{regexp} is a string to compile it internally).
;;
;;-sig: (regexp [bound [noerror [repeat]]])
;;
(define (gb-re-search-forward gb regexp . args)
  (let* ((p (gb-point gb))
         (pmax (gb-point-max gb)))
    (receive (bound fail reps) (parse-search-args gb pmax args regexp)
      (if (and (string? regexp) (string=? "" regexp))
          p
          (let* ((s (s: gb))
                 (start (aft-ofs: gb))
                 (rx (if (regexp? regexp)
                         regexp
                         (make-regexp regexp regexp/newline)))
                 (next (lambda (from flag)
                         (regexp-exec rx s from flag)))) ; ugh
            (let loop ((amatch (next start (if (= p 1) 0 regexp/notbol)))
                       (reps (1- reps)))
              (cond
               ;; no match / done
               ((not amatch) (fail))
               ;; match / done if `reps' satisfied
               ((= 0 reps)
                (md! gb (guile-md->md amatch (1+ (- (gap-ofs: gb) start))))
                (let ((rest-point (gb-match-end gb)))
                  (gb-goto-char gb rest-point)
                  rest-point))          ; retval
               ;; match / not done
               (else (loop (next (match:end amatch) regexp/notbol)
                           (1- reps))))))))))

(define (replacement-text gb newtext)
  (define (err!) (error "invalid use of `\\' in replacement text"))
  (let* ((last-char (1- (string-length newtext)))
         (c-zero-as-num (char->integer #\0)))
    (define (nrep n) (gb-match-string gb n))
    (define (crep c) (nrep (- (char->integer c) c-zero-as-num)))
    (let loop ((start 0) (acc '()))
      (cond ((string-index newtext #\\ start)
             => (lambda (cut)
                  (and (= cut last-char) (err!))
                  (let ((pre (substring newtext start cut))
                        (c2 (string-ref newtext (1+ cut))))
                    (case c2
                      ((#\&) (loop (+ 2 cut) (list* (nrep 0) pre acc)))
                      ((#\\) (loop (+ 2 cut) (list* "\\" pre acc)))
                      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                       (loop (+ 2 cut) (list* (crep c2) pre acc)))
                      (else (err!))))))
            (else
             (apply string-append
                    (reverse! (cons (substring newtext start) acc))))))))

;; Replace text matched by last search with @var{newtext}.
;; The second arg is optional and ignored (for now -- in the
;; future it may specify case handling a la Emacs).
;;
;; If third arg @var{literal} is non-#f, insert @var{newtext} literally.
;; Otherwise treat @code{\} as special:
;; @example
;;   `\&' in NEWTEXT means substitute original matched text.
;;   `\N' means substitute what matched the Nth `(...)'.
;;        If Nth parens didn't match, substitute nothing.
;;   `\\' means insert one `\'.
;; @end example
;;
;; Leave point at end of replacement text.
;;
;;-sig: (newtext [IGNORED [literal]])
;;
(define (gb-replace-match gb newtext . args)
  (check-read-only gb)
  (let* ((m (md gb)) (buhbye (list-ref m 0))
         (rtxt (if (or (and (< 1 (length args)) (cadr args))
                       (not (string-index newtext #\\))) ; optimize
                   newtext              ; literal
                   (replacement-text gb newtext))))
    (gb-goto-char gb (cdr buhbye))
    (gb-delete-char! gb (- (car buhbye) (cdr buhbye)))
    (gb-insert-string! gb rtxt)))

;;; i/o and bulk transforms

;; Send the contents of @var{gb} to the output @var{port}.
;; Optional args @var{beg} and @var{end} specify a region to send.
;; Point does not move.
;;
;;-sig: (gb port [beg [end]])
;;
(define (gb->port! gb port . opts)
  (let* ((s   (s: gb))
         (len (string-length s))
         (gap (gap-ofs: gb))
         (aft (aft-ofs: gb)))
    (define (chunk->port! start end)
      (let loop ((this-time (write-string/partial s port start end))
                 (so-far start))
        (and (< (+ so-far this-time) end)
             (loop (write-string/partial s port (+ so-far this-time) end)
                   (+ so-far this-time)))))
    (cond ((null? opts)
           (chunk->port! 0 gap)
           (chunk->port! aft len))
          (else
           (let* ((beg (1- (car opts)))
                  (end (if (null? (cdr opts))
                           len
                           (let ((v (1- (cadr opts))))
                             (cond ((< gap v)
                                    (+ v (- aft gap)))
                                   (else
                                    ;; adjust first chunk end point
                                    (set! gap v)
                                    #f))))))
             (chunk->port! beg gap)
             (and end (chunk->port! aft end)))))))

;; Return a new string representing the text of @var{gb}.
;; Point does not move.
;;
(define (gb->string gb)
  (let* ((s   (s: gb))
         (len (string-length s))
         (gap (gap-ofs: gb))
         (aft (aft-ofs: gb))
         (rv  (make-string (+ gap (- len aft)))))
    (substring-move! s 0 gap rv 0)
    (substring-move! s aft len rv gap)
    rv))

(define (bsub gb x y)                   ; x and y are 0-based
  (let ((s (s: gb))
        (gap (gap-ofs: gb)))
    (if (< y gap)
        (substring s x y)
        (let ((aft (aft-ofs: gb)))
          (if (< gap x)
              (substring s (+ (- x gap) aft) (+ (- y gap) aft))
              (string-append (substring s x gap)
                             (substring s aft (+ (- y gap) aft))))))))

;; Return the region of @var{gb} from @var{start} to @var{end} as a string.
;;
(define (gb->substring gb start end)
  (or (<= start end) ((lambda (new-end new-start)
                        (set! start new-start)
                        (set! end new-end))
                      start end))
  (or (<= (gb-point-min gb) start)
      (error "arg out of range:" start))
  (or (<= end (gb-point-max gb))
      (error "arg out of range:" end))
  (bsub gb (1- start) (1- end)))

;; Pass the string representing the text of @var{gb} to @var{string-proc} and
;; use its return value to completely replace the contents of @var{gb}.
;; Point is left at the maximum position.
;;
(define (gb-filter! gb string-proc)
  (check-read-only gb)
  (let ((new (string-proc (gb->string gb))))
    (gb-erase! gb)
    (gb-insert-string! gb new)))

;; Return a list of strings representing the lines of text of @var{gb}.
;; Newlines are automatically removed.  A buffer with N newlines results
;; in a list of length N+1.  Point does not move.
;;
(define (gb->lines gb)
  (let ((s (s: gb))
        (rv '()))
    (define (add! line) (set! rv (cons line rv)))
    (define (splice! line) (set-car! rv (string-append line (car rv))))
    (define (->lines! check-splice? end beg)
      (let loop ((rpos end) (just-found? #f))
        (define (sub lpos)
          (substring s lpos rpos))
        (cond ((string-rindex s #\newline beg rpos)
               => (lambda (i)
                    ((cond ((and check-splice? (not (null? rv)))
                            (set! check-splice? #f)
                            (if (= (1- end) i)
                                identity
                                splice!))
                           (else add!))
                     (sub (1+ i)))
                    (loop i #t)))
              ((and (= beg rpos) check-splice? (not just-found?)))
              (else ((if check-splice?
                         splice!
                         add!)
                     (sub beg))))))
    ;; do it!
    (->lines! #f (string-length s) (aft-ofs: gb))
    (->lines! #t (gap-ofs: gb) 0)
    rv))

;; Pass the list of strings representing the lines of text of @var{gb} to
;; @var{lines-proc} and use its return value (another list of strings) to
;; completely replace the contents of @var{gb}.  Newlines are automatically
;; removed and added back.  Point is left at the maximum position.
;;
(define (gb-filter-lines! gb lines-proc)
  (check-read-only gb)
  (let ((ls (lines-proc (gb->lines gb))))
    (gb-erase! gb)
    (or (null? ls)
        (let* ((lengths (map string-length ls))
               (gap-ofs (insert-prep gb (apply + (length ls) -1 lengths)))
               (s (s: gb)))
          ;; first
          (let* ((head (car ls))
                 (hlen (car lengths)))
            (substring-move! head 0 hlen s gap-ofs)
            (set! gap-ofs (+ hlen gap-ofs)))
          ;; rest
          (for-each (lambda (head hlen)
                      (string-set! s gap-ofs #\newline)
                      (substring-move! head 0 hlen s (1+ gap-ofs))
                      (set! gap-ofs (+ 1 hlen gap-ofs)))
                    (cdr ls)
                    (cdr lengths))
          ;; update
          (gap-ofs! gb gap-ofs)))))

;; Return a "soft port" on @var{gb} that supports the write-character,
;; write-string and read-character operations (flush-output and close-port
;; are not supported).  All operations move point forward.  Additionally,
;; writing operations increase the value that would be returned by
;; @code{gb-point-max}.
;;
(define (make-gap-buffer-port gb)
  (or (gb? gb)
      (error "not a gap-buffer:" gb))
  (make-soft-port
   (vector
    (lambda (c) (gb-insert-char! gb c))
    (lambda (s) (gb-insert-string! gb s))
    #f
    (lambda () (let ((gap-ofs (gap-ofs: gb))
                     (aft-ofs (aft-ofs: gb))
                     (s       (s: gb)))
                 (if (= aft-ofs (string-length s))
                     #f
                     (let ((c (string-ref s aft-ofs)))
                       (string-set! s gap-ofs c)
                       (gap-ofs! gb (1+ gap-ofs))
                       (aft-ofs! gb (1+ aft-ofs))
                       c))))
    #f)
   "rw"))

;;; gap-buffer.scm ends here
