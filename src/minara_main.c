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

/*
  Introduce making windows to Guile...
*/

/*-----------------------------------------------------------------------------
  Includes    
  ---------------------------------------------------------------------------*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libguile.h"

#ifdef __APPLE__
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <GLUT/glut.h>
#else
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#endif

#include "minara_events.h"
#include "minara_guile.h"
#include "minara_rendering.h"
#include "minara_cache.h"
#include "minara_window.h"

/*-----------------------------------------------------------------------------
  Constants    
  ---------------------------------------------------------------------------*/

static const char * kBootstrapFile = "../lisp/minara-bootstrap.scm";

/*-----------------------------------------------------------------------------
  Globals    
  ---------------------------------------------------------------------------*/


/*-----------------------------------------------------------------------------
  Functions
  ---------------------------------------------------------------------------*/

// Program lifecycle

/**
   The procedure called by guile as our real main.
   Initialises SDL and guile, runs the main event loop,
   shuts down SDL and guile, then exits.
*/

void RealMain () {
  // Register all the Guile extensions
  GuileStartup ();
  RenderingStartup ();
  CacheStartup ();
  WindowStartup ();
  EventsStartup (); 
  // Bootstrap the Guile code (libraries, tools, etc.)
  // Here so all the C extensions are loaded first and GLUT is initialised
  scm_primitive_load_path ( scm_makfrom0str (kBootstrapFile));
  // Main event loop
  glutMainLoop ();
  // We quit here
  exit (0);
}

/**
   Our main. Just copies arc/v and calls Guile with our RealMain .
*/

int main (int argc, char ** argv) {
  // Init GLUT
  glutInit (&argc, argv);
  // never returns
  scm_boot_guile (argc, argv, RealMain, NULL);
  // Keep the compiler happy...
  return 0;
}
