/*
  minara - a programmable graphics program editor
  Copyright (C) 2004  Rob Myers rob@robmyers.org

  Some code
  Copyright (C) 1995,1996,1997,1998,1999,2000,2001 Free Software Foundation, Inc.

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
  Scheme
  
  Our Lisp system. We currently use Guile, so it's actually our Scheme system.
  This should still be called minara_lisp.c, though.
  The code in this file extends Guile to allow us to evaluate code buffers 
  exactly as we need to for minara to function as planned.
  ---------------------------------------------------------------------------*/

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

char *guile_do_nothinevent_handlers = \
"(define %quit-hook (lambda () (write-line \"quit-hook\" (current-error-port)) (force-output (current-error-port)))) \
  (define %resize-hook (lambda (win width height) (write-line \"resize-hook\" (current-error-port)) (force-output (current-error-port)))) \
  (define %draw-hook (lambda (win) (write-line \"draw-hook\" (current-error-port)) (force-output (current-error-port)))) \
  (define %mouse-down-hook (lambda (win button x y) (write-line \"mouse-down-hook\" (current-error-port)) (force-output (current-error-port)))) \
  (define %mouse-up-hook (lambda (win button x y) (write-line \"mouse-up-hook\" (current-error-port)) (force-output (current-error-port)))) \
  (define %mouse-move-hook (lambda (win x y) (write-line \"mouse-move-hook\" (current-error-port)) (force-output (current-error-port)))) \
  (define %key-press-hook (lambda (win key modifiers) (write-line \"key-press-hook\" (current-error-port)) (force-output (current-error-port)))) \
  (define %key-release-hook (lambda (win key modifiers) (write-line \"key-release-hook\" (current-error-port)) (force-output (current-error-port)))) \
  (define %menu-select-hook (lambda (win menu-id) (write-line \"menu-select-hook\" (current-error-port)) (force-output (current-error-port))))";


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

void
define_config_module ()
{  
  // Set the guile path
  scm_c_define ("$minara-lisp-dir", 
		scm_makfrom0str (MINARA_LISP_DIR));
  scm_c_define ("$minara-dotminara-dir", 
		scm_makfrom0str (MINARA_DOTMINARA_DIR));
  //Export them
  scm_c_export ("$minara-lisp-dir", "$minara-dotminara-dir", NULL);
}

void
guile_startup ()
{
  //Define our module
  scm_c_define_module ("minara-internal config", define_config_module, NULL);

  //Ensure we have do-nothing event handlers installed
  // Now done in the Scheme code (see lisp / events.scm)
  // scm_c_eval_string (gGuileDoNothingEventHandlers);
  //Add our extensions to % load - entensions in scheme
}
