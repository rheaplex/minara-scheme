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
  Functions
  ---------------------------------------------------------------------------*/

//Scheme functions

/*
 * Evaluating strings in modules
 * One of the
 * foundations of Minara is the idea that the same code can be evaluated with
 * different bindings for the same functions. So line-to has a different
 * effect when rendering and picking for example. We could set! the function
 * bindings before each render/pick, we could have the functions dispatch
 * differently depending on a global (bound by a macro for the duration of
 * render/pick then restored). But I wanted to try it like this. Better
 * implementations are welcomed. :-) - robmyers.
 */

/**
   Evaluate a string port. Copied from strport.c in libguile.
   @param port The port to read the Scheme code from to evaluate
   @param module The module to evaluate the code in
   @return The result of evaluation
*/

SCM our_inner_eval_string (void * port)
{
  SCM form;
  SCM result = SCM_UNSPECIFIED;

  /* Read expressions from that port; ignore the values.  */
  while (!SCM_EOF_OBJECT_P (form = scm_read ((SCM)port)))
    result = scm_primitive_eval_x (form);

  /*
   * Don't close the port here; if we re-enter this function via a
   * continuation, then the next time we enter it, we'll get an error.
   * It's a string port anyway, so there's no advantage to closing it
   * early.
   */

  return result;
}

/**
   Evaluate a port in the given module
   @param port The prot to read the Scheme code from to evaluate
   @param module The module to evaluate the code in
   @return The result of evaluation
*/

SCM
minara_port_eval_with_module (SCM port, SCM module)
{
  return scm_c_call_with_current_module (module, our_inner_eval_string, (void *) port);
}

/**
   Evaluate a port in the given module
   @param port The prot to read the Scheme code from to evaluate
   @param module The module to evaluate the code in
   @return The result of evaluation
*/

SCM
minara_strineval_with_module (SCM string, SCM module)
{
  SCM port = scm_mkstrport (SCM_INUM0, string, SCM_OPN | SCM_RDNG,
			    "eval-string");
  return minara_port_eval_with_module (port, module);
}


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
guile_startup ()
{
  // Set the guile path
  scm_c_define ("$minara-lisp-dir", 
		scm_makfrom0str (MINARA_LISP_DIR));
  scm_c_define ("$minara-dotminara-dir", 
		scm_makfrom0str (MINARA_DOTMINARA_DIR));

  //Register our scheme functions
  scm_c_define_gsubr ("port-eval-with-module", 2, 0, 0, 
		      minara_port_eval_with_module);
  scm_c_define_gsubr ("string-eval-with-module", 2, 0, 0, 
		      minara_strineval_with_module);

  //Ensure we have do-nothing event handlers installed
  // Now done in the Scheme code (see lisp / events.scm)
  // scm_c_eval_string (gGuileDoNothingEventHandlers);
  //Add our extensions to % load - entensions in scheme
  }
