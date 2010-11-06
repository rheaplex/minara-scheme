;;; split-string-no-nulls --- Split a string like Emacs to stdout

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

;; Usage: split-string-no-nulls STRING [SEPARATORS]
;;
;; Split STRING using " \f\t\n\r\v" to find boundaries, and display
;; substrings one per line to stdout.  Optional arg SEPARATORS is a
;; string to use besides the default (you may need to quote it to
;; protect from the shell).  This mimics Emacs Lisp `split-string',
;; with the difference that when SEPARATORS is specified, Emacs
;; preserves nulls while `split-string-no-nulls' does not.  For
;; example, given the string S "abc  def" (two spaces), we see:
;;
;;  Emacs (split-string S)     => ("abc" "def")
;;  Emacs (split-string S " ") => ("abc" "" "def")
;;
;; versus
;;
;;  (split-string-no-nulls S)     => ("abc" "def")
;;  (split-string-no-nulls S " ") => ("abc" "def")
;;
;;
;; Usage from a Scheme program:
;;  (split-string-no-nulls STRING [SEPARATORS])
;;  (split-string-no-nulls-proc SEPARATORS)
;;
;; This first procedure returns the list of split strings instead of
;; sending them to stdout.  The second returns a procedure that takes
;; a string and does the splitting (use this to "compile once, use
;; many times").

;;; Code:

(define-module (scripts split-string-no-nulls)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:export (split-string-no-nulls
            split-string-no-nulls-proc))

(define emacs-separators-charset-complement
  (char-set-complement (string->char-set " \f\t\n\r\v")))

(define (split-string-no-nulls string . separators)
  (string-tokenize string
                   (if (null? separators)
                       emacs-separators-charset-complement
                       (char-set-complement
                        (string->char-set (car separators))))))

(define (split-string-no-nulls-proc separators)
  (let ((cs-comp (char-set-complement (string->char-set separators))))
    (lambda (string)
      (string-tokenize string cs-comp))))

;;; split-string-no-nulls ends here
