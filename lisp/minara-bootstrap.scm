;; minara-bootstrap.scm : load the support files and call startup
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

;; Load third party libraries

(load "./slurp.scm")
(load "./split-string-no-nulls.scm")
(load "./fileutils.scm")
(load "./expand-file-name.scm")
(load "./gap-buffer.scm")
(load "./find-file.scm")

;; Load our main loading code (migrate this to libraries and this file)

(load "./minara.scm")

;; Call startup from minara.scm

(startup (program-arguments))