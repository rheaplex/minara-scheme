;; fill.scm : handling the current fill colour for minara
;;
;; Copyright (c) 2004 Rob Myers, rob@robmyers.org
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
;; Fill colour handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the fill
;; Ultimately the fill will be a shader, but for now it's an rgb triple
;; We use littlecms to translate colours
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Standard functions to translate colours to RGBA tuples
;; Implement and export from littlecms
(xyz (x y z) (list x y z))
(rgb (r g b) (rgb->xyz r g b))
(hsv (h s v) (hsv->xyz h s v))
(lab (l a b) (lab->xyz h s v))

;; Iterate over the components, calling each to convert to RGBA
;; and sum the results
;; What happens if the result goes off the end of the scale or out of gamut?
(fill (&rest all)
      (let ((r 0.0)
            (g 0.0)
            (b 0.0)
            (a 0.0))
        (maplist (f the-fill)
                 (let ((rr gg bb aa) (f))
                   (setf r (+ (rr * aa) r))
                   (setf g (+ (* gg aa) g))
                   (setf b (+ (* bb aa) b))
                   (setf a (+ aa a)))
                 (list r g b))))

;; Components are ink/phosphor/whatever gamut limits
;; For example sRGB R will be R:0.76 or whatever
;; Use XYZ instead?
;; We use lcms to convert the value for the actual device percentage amount?
