/*
  minara - a programmable graphics program editor
  Copyright (C) 2004  Rob Myers rob@robmyers.org

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/*-----------------------------------------------------------------------------
  Includes  
  ---------------------------------------------------------------------------*/

#include <stdlib.h>

#include <libguile.h>

#include "minara_guile.h"

/*-----------------------------------------------------------------------------
  Global Variables
  ---------------------------------------------------------------------------*/

/** Do-nothing event handlers. */

char * gGuileDoNothingEventHandlers = \
  "(define %quit-hook (lambda () (write-line \"quit-hook\" (current-error-port)) (force-output (current-error-port)))) \
  (define %resize-hook (lambda (win width height) (write-line \"resize-hook\" (current-error-port)) (force-output (current-error-port)))) \
  (define %draw-hook (lambda (win) (write-line \"draw-hook\" (current-error-port)) (force-output (current-error-port)))) \
  (define %mouse-down-hook (lambda (win button x y) (write-line \"mouse-down-hook\" (current-error-port)) (force-output (current-error-port)))) \
  (define %mouse-up-hook (lambda (win button x y) (write-line \"mouse-up-hook\" (current-error-port)) (force-output (current-error-port)))) \
  (define %mouse-move-hook (lambda (win x y) (write-line \"mouse-move-hook\" (current-error-port)) (force-output (current-error-port)))) \
  (define %key-press-hook (lambda (win key) (write-line \"key-press-hook\" (current-error-port)) (force-output (current-error-port))))";

/*-----------------------------------------------------------------------------
  Program Lifecycle
  
  This is the main Guile setup code.
  Extensions are scattered throughout the code, notably in 
  minara_rendering.c .
  The main program lifecycle code that calls into Guile is
  found in minara_main.c .
  ---------------------------------------------------------------------------*/

/**
 * Load the library, configuration, tool and user startup files
 * into the top level environment
 */

void GuileStartup () {
  // Ensure we have do-nothing event handlers installed
  scm_c_eval_string (gGuileDoNothingEventHandlers);
  // Add our extensions to %load-entensions in scheme
  // Load our library code from ?
  // Load .minara without searching, and without failing if it isn't there
  //scm_primitive_load ( scm_makfrom0str ("~/.minara"));
}
