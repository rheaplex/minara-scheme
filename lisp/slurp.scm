;;; slurp --- Read a file into a string

;; 	Copyright (C) 2002,2003 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA
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

;; Usage: slurp FILE ...
;;
;; Read all FILEs into a string and display it to stdout, like cat(1).
;;
;; Usage from a Scheme program:
;;  (slurp stuff) => string
;;  (slurp-file! buf fname-or-port rd-offset len wr-offset) => buf
;;
;; STUFF is either a list of filenames, each a string, or a single filename.
;;
;; Unlike `slurp', which allocates a new string, the lower-level procedure
;; `slurp-file!' modifies (and returns) BUF, a caller-allocated string.
;; FNAME-OR-PORT is a single string, or a seekable port; and RD-OFFSET, LEN
;; and WR-OFFSET are integers specifying where to start reading in the file,
;; how many bytes to read, and where to start writing in BUF, respectively.

;;; Code:

(define-module (scripts slurp)
  #:autoload (ice-9 rw) (read-string!/partial)
  #:export (slurp slurp-file!))

(define (slurp-file! buf fname-or-port rd-offset len wr-offset)
  (let* ((p? (port? fname-or-port))
         (port (if p? fname-or-port (open-input-file fname-or-port)))
         (end (+ wr-offset len))
         (smore-please (lambda (start)
                         (read-string!/partial buf port start end))))
    (seek port rd-offset SEEK_SET)
    (let loop ((this-time (smore-please wr-offset))
               (so-far 0))
      (and (< (+ so-far this-time) len)
           (loop (smore-please (+ so-far wr-offset))
                 (+ so-far this-time))))
    (or p? (close-port port)))
  buf)

(define (slurp stuff)
  (cond ((list? stuff)
         (let* ((tot 0)
                (fsw (map (lambda (filename)
                            (let ((size (stat:size (stat filename)))
                                  (wr-offset tot))
                              (set! tot (+ tot size))
                              ;; "fsw": short for filename/size/wr-offset
                              (list filename 0 size wr-offset)))
                          stuff))
                (buf (make-string tot))
                (go! (lambda (fsw)
                       (apply slurp-file! buf fsw))))
           (for-each go! fsw)
           buf))
        ((string? stuff)
         (let ((size (stat:size (stat stuff))))
           (slurp-file! (make-string size) stuff 0 size 0)))
        (else
         ;; there is never enough drug humor in the world
         (error "bad stuff (dude)!"))))

;;; slurp ends here
