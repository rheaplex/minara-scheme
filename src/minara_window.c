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
  NOTES.
  We represent a window as an integer ID, the same as the GLUT id.
  This isn't typesafe but is efficient, and we need to pass the
  ID into the event loop from C code, so it's for the best.
*/


/*-----------------------------------------------------------------------------
  Includes  
  ---------------------------------------------------------------------------*/

#include <stdio.h>

#include <libguile.h>

#ifdef __APPLE__
#include <OpenGL/gl.h>
#include <OpenGL/glext.h>
#include <OpenGL/glu.h>
#include <GLUT/glut.h>
#else
#include <GL/gl.h>
#include <GL/glext.h>
#include <GL/glu.h>
#include <GL/glut.h>
#endif

#include "minara_events.h"

#include "minara_window.h"

/*-----------------------------------------------------------------------------
  Local Prototypes
  ---------------------------------------------------------------------------*/

static void GlutWindowSet (int win);

/*-----------------------------------------------------------------------------
  Global Variables
  ---------------------------------------------------------------------------*/

// Get these from the launch environment
/** The window startup width */
int gScreenWidth = 640;
/** The window startup height */
int gScreenHeight = 480;

/*-----------------------------------------------------------------------------
  Functions
  ---------------------------------------------------------------------------*/

// Utilities

/*
  Set the window, allowing for being passed an invalid window.
  @param win The window id. If zero, nothing will be done.
*/

static void GlutWindowSet (int win) {
  if (win != 0) {
    glutSetWindow (win);
  }
}

// Scheme methods
/**
   Make a window, set up the callbacks.
   @return The window ID or '()
*/

SCM minara_window_make () {
  int win = 0;
  glutInitDisplayMode (GLUT_SINGLE | GLUT_RGB);
  glutInitWindowSize (gScreenWidth, gScreenHeight); 
  //glutInitWindowPosition (0, 0); 
  win = glutCreateWindow ("");
  if (win == 0) {
    return SCM_EOL;
  }
  // Install the event handlers
  glutReshapeFunc (GlutResize);
  glutDisplayFunc (GlutDisplay);
  glutKeyboardFunc (GlutKeyPress);
  glutMouseFunc (GlutMouseButton);
  glutMotionFunc (GlutMouseDrag);
  glutPassiveMotionFunc (GlutMouseMove);

  // Return the new window ID
  return scm_int2num(win);
}


/**
   Dispose of a window's OS resources.
   @param window The window to finalise and deallocate.
   @return '()
*/

SCM minara_window_dispose (SCM window) {
  GLuint win = 0;
  GLuint oldWin = glutGetWindow ();
  SCM_ASSERT(SCM_NUMBERP(window), window, SCM_ARG1, "minara-window-dispose");
  win = (GLuint)scm_num2int (window, SCM_ARG1, "minara-window-dispose");
  GlutWindowSet (win);
  glutHideWindow ();
  GlutWindowSet (oldWin);
  glutDestroyWindow (win);
  return SCM_EOL;
}

/**
   Get the current window.
   @return A fresh smob for the window, don't (dispose)!
*/

SCM minara_window_current () {
  return scm_uint2num (glutGetWindow ());
}

/**
   Set the current window.
   @param win The window ID to set as current.
   @return '()
*/

SCM minara_window_set (SCM win) {
  GLuint w = 0;
  SCM_ASSERT(SCM_NUMBERP(win), win, SCM_ARG1, "minara-window-set");
  w = (GLuint)scm_num2int (win, SCM_ARG1, "minara-window-set");
  GlutWindowSet (w);
  return SCM_EOL;
}

// TODO:

// Window name getter and setter
// Window position and size getter and setter


// Utilities

SCM MinaraWindowCurrent () {
  return minara_window_current ();
}


// Program lifecycle

/**
   Register our Guile functions
*/

void WindowStartup () {
  // Register our scheme functions
  scm_c_define_gsubr ("window-make", 0, 0, 0, minara_window_make);
  scm_c_define_gsubr ("window-dispose", 0, 0, 0, minara_window_dispose);
  scm_c_define_gsubr ("window-current", 0, 0, 0, minara_window_current);
  scm_c_define_gsubr ("window-set", 1, 0, 0, minara_window_set);
}
