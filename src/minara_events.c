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
    Make sure that mouse move/drag events only flow when the mouse pointer is
    in a window?
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

#include "minara_guile.h"
#include "minara_window.h"

/*-----------------------------------------------------------------------------
  Global Variables
  ---------------------------------------------------------------------------*/

/** The current quit event scheme hook */
SCM gQuitHook;
/** The current window resize event scheme hook */
SCM gResizeHook;
/** The current window expose event scheme hook */
SCM gDrawHook;
/** The current mouse button event scheme hook */
SCM gMouseButtonDownHook;
/** The current mouse button release event scheme hook */
SCM gMouseButtonUpHook;
/** The current mouse moved event scheme hook */
SCM gMouseMoveHook;
/** The current key pressed event scheme hook */
SCM gKeyPressHook;
/** The current menu selected event scheme hook */
SCM gMenuSelectHook;

/*-----------------------------------------------------------------------------
  Functions
  ---------------------------------------------------------------------------*/

// Callback and Scheme interfacing

/**
   (Re)bind the C hooks to the Scheme callbacks.
   If the Scheme code has changed these, the C bindings will point to the old
   versions, so this needs re-doing whenever the C or the Scheme changes the
   hooks.
*/

void BindEventHooks () {
  gQuitHook = scm_c_eval_string ("%quit-hook");
  gResizeHook = scm_c_eval_string ("%resize-hook");
  gDrawHook = scm_c_eval_string ("%draw-hook");
  gMouseButtonDownHook = scm_c_eval_string ("%mouse-down-hook");
  gMouseButtonUpHook = scm_c_eval_string ("%mouse-up-hook");
  gMouseMoveHook = scm_c_eval_string ("%mouse-move-hook");
  gKeyPressHook = scm_c_eval_string ("%key-press-hook");
  gMenuSelectHook = scm_c_eval_string ("%menu-select-hook");
}

/**
   Tools call this to hook themselves into the system.
   So they set the relevent hook, eg %draw-hook, then call this.
   @return A SCM for guile, ignore.
*/

SCM scm_bind_event_hooks() {
  BindEventHooks();
  return SCM_EOL;
}

/**
   Register the guile extensions used in managing the main event loop
*/

void EventsStartup () {
  scm_c_define_gsubr ("bind-event-hooks", 0, 0, 0, scm_bind_event_hooks);
  // called in events.scm
  //BindEventHooks ();
}


// Event hook calling

/**
   The window redraw callback for GLUT.
*/

void GlutDisplay () {
  // This may change as the renderer evolves
  glShadeModel (GL_FLAT);
  //TODO: Anti-aliasing. Allow enabling/disabling from Scheme/preferences
  // Disable costly functions
  //    (most are disabled anyway)
  glDisable (GL_DITHER);
  glDisable (GL_DEPTH_TEST);
  glClearColor (1.0, 1.0, 1.0, 1.0);
  glClear (GL_COLOR_BUFFER_BIT);
  scm_call_1 (gDrawHook, MinaraWindowCurrent ());
  glFlush ();
  glutSwapBuffers();
}

/**
   The window resize callback for GLUT.
   @param width The new width of the window in pixels.
   @param height The new height of the window in pixels.
*/

void GlutResize (int width, int height) {
  // Reshape the OpenGL viewport
  glViewport (0, 0, (GLsizei)width, (GLsizei)height);
  glMatrixMode (GL_PROJECTION);
  glLoadIdentity ();
  gluOrtho2D (0.0, (GLdouble)width, 0.0, (GLdouble)height);
  glMatrixMode (GL_MODELVIEW);
  glLoadIdentity ();
  // Tell guile
  scm_call_3 (gResizeHook, MinaraWindowCurrent (),
  	      scm_long2num (width),
	      scm_long2num (height));
  
}

/**
   The mouse motion callback for GLUT.
   @param key The ascii key code.
   @param x The x co-ordinate the mouse is at.
   @param y The y co-ordinate the mouse is at.
*/

void GlutKeyPress (unsigned char key, int x, int y) {
  int modifiers = glutGetModifiers ();
  char keyString[] = {key, NULL};
  scm_call_3 (gKeyPressHook, MinaraWindowCurrent (),
	      scm_makfrom0str (keyString), scm_long2num (modifiers));
}

/**
   The mouse button press callback for GLUT.
   @param button The button: left, middle or right.
   @param state Whether the button has been pressed or released.
   @param x The x co-ordinate the mouse is at.
   @param y The y co-ordinate the mouse is at.
*/

void GlutMouseButton (int button, int state, int x, int y) {
  int buttonNum;
  switch (button) {
  case GLUT_LEFT_BUTTON:
    buttonNum = 1;
    break;
  case GLUT_MIDDLE_BUTTON:
    buttonNum = 2;
    break;
  case GLUT_RIGHT_BUTTON:
    buttonNum = 3;
    break;
  }
  if (state == GLUT_UP) {
    scm_call_4 (gMouseButtonUpHook, MinaraWindowCurrent (),
		scm_long2num(buttonNum), 
		scm_long2num (x), scm_long2num (y));
  } else {
    scm_call_4 (gMouseButtonDownHook, MinaraWindowCurrent (),
		scm_long2num(buttonNum), 
		scm_long2num (x), scm_long2num (y));
  }
}

/**
   The mouse drag (move with buttons down) callback for GLUT.
   @param x The x co-ordinate the mouse is now at.
   @param y The y co-ordinate the mouse is now at.
*/

void GlutMouseDrag (int x, int y) {
  scm_call_3 (gMouseMoveHook, MinaraWindowCurrent (),
	      scm_long2num (x), scm_long2num (y));
}

/**
   The mouse motion callback for GLUT.
   @param x The x co-ordinate the mouse is now at.
   @param y The y co-ordinate the mouse is now at.
*/

void GlutMouseMove (int x, int y) {
  scm_call_3 (gMouseMoveHook, MinaraWindowCurrent (),
	      scm_long2num (x), scm_long2num (y));
}

/**
   The menu select callback for GLUT.
   @param id The menu id number.
*/

void GlutMenuSelect (int id) {
  scm_call_2 (gMenuSelectHook, MinaraWindowCurrent (),
	      scm_long2num (id));
}

