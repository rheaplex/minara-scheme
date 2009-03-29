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
  Windows
  
  Windows. Think of them as Mac windows, not MDI windows.
  
  These are GLUT windows at present, but could be Cocoa or GTK windows.
  
  We do not have multiple views on a single document at the moment. The model
  of a stack of buffers around a document buffer may interact with this.
  ---------------------------------------------------------------------------*/

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
#include <string.h>

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

static void glut_window_set (int win);

/*-----------------------------------------------------------------------------
  Functions
  ---------------------------------------------------------------------------*/

//Utilities

/*
  Set the window, allowing for being passed an invalid window.
  @param win The window id. If zero, nothing will be done.
*/

static void
glut_window_set (int win)
{
  if (win != 0)
  {
    glutSetWindow (win);
  }
}

//Scheme methods

/**
   Make a window, set up the callbacks.
   @return The window ID or '()
*/

SCM minara_window_make (SCM width, SCM height)
{
  int win = 0;
  int w;
  int h;
  SCM_ASSERT (SCM_NUMBERP (width), width, SCM_ARG1, "minara_window_make");
  SCM_ASSERT (SCM_NUMBERP (height), height, SCM_ARG2, "minara_window_make");
  w = scm_num2dbl (width, "minara_window_make");
  h = scm_num2dbl (height, "minara_window_make");

  glutInitDisplayMode (GLUT_DOUBLE | GLUT_RGB);
  glutInitWindowSize (w, h);
  //glutInitWindowPosition (0, 0);
  win = glutCreateWindow ("");
  if (win == 0)
  {
    return SCM_EOL;
  }
  //Install the event handlers
  glutReshapeFunc (glut_resize);
  glutDisplayFunc (glut_display);
  glutKeyboardFunc (glut_key_press);
  glutKeyboardUpFunc (glut_key_release);
  glutMouseFunc (glut_mouse_button);
  glutMotionFunc (glut_mouse_drag);
  glutPassiveMotionFunc (glut_mouse_move);
  glutAttachMenu (GLUT_RIGHT_BUTTON);
  //Return the new window ID
    return scm_int2num (win);
}


/**
   Dispose of a window's OS resources.
   @param window The window to finalise and deallocate.
   @return '()
*/

SCM
minara_window_dispose (SCM window)
{
  GLuint win = 0;
  GLuint old_win = glutGetWindow ();
  SCM_ASSERT (SCM_NUMBERP (window), window, SCM_ARG1, "minara-window-dispose");
  win = (GLuint) scm_num2int (window, SCM_ARG1, "minara-window-dispose");
  glut_window_set (win);
  glutHideWindow ();
  glut_window_set (old_win);
  glutDestroyWindow (win);
  return SCM_EOL;
}

/**
   Get the current window.
   @return A fresh smob for the window, don't (dispose)!
*/

SCM
minara_window_current ()
{
  return scm_uint2num (glutGetWindow ());
}

/**
   Set the current window.
   @param win The window ID to set as current.
   @return '()
*/

SCM
minara_window_set (SCM win)
{
  GLuint w = 0;
  SCM_ASSERT (SCM_NUMBERP (win), win, SCM_ARG1, "minara-window-set");
  w = (GLuint) scm_num2int (win, SCM_ARG1, "minara-window-set");
  glut_window_set (w);
  return SCM_EOL;
}

/**
   Tell the window to redraw.
   @param win The window id to redraw.
   @return '()
*/

SCM
minara_window_invalidate (SCM win)
{
  GLuint w = 0;
  SCM_ASSERT (SCM_NUMBERP (win), win, SCM_ARG1, "minara-window-invalidate");
  w = (GLuint) scm_num2int (win, SCM_ARG1, "minara-window-invalidate");
  glutPostWindowRedisplay (w);
  return SCM_EOL;
}

//TODO:

//Window position and size getter and setter

/**
   Set the window title.
   @param win The window id to set the title of.
   @param title The window title string.
   @return '()
*/

SCM minara_window_set_title (SCM win, SCM title)
{
  GLuint w = 0;
  char *t = NULL;
  int old_win = glutGetWindow ();
  SCM_ASSERT (SCM_NUMBERP (win), win, SCM_ARG1, "minara-window-set-title");
  w = (GLuint) scm_num2int (win, SCM_ARG1, "minara-window-set-title");
  SCM_ASSERT (scm_is_string (title), title, SCM_ARG2, 
	      "minara-window-set-title");
  t = scm_to_locale_string (title);
  if (w != 0)
  {
    glutSetWindow (w);
    glutSetWindowTitle (t);
  }
  if (old_win != 0)
    glutSetWindow (old_win);
  free (t);
  return SCM_EOL;
}


/**
   Draw (or set, depending on the API) the window status information.
   @param win The window id to draw the status string on.
   @param title The window status string.
   @return '()
*/

SCM minara_window_draw_status(SCM win, SCM text)
{
  GLuint w = 0;
  char *status = NULL;
  int old_win = glutGetWindow ();
  SCM_ASSERT (SCM_NUMBERP (win), win, SCM_ARG1, "minara-window-set-status");
  w = (GLuint) scm_num2int (win, SCM_ARG1, "minara-window-set-status");
  SCM_ASSERT (scm_is_string (text), text, SCM_ARG2, "minara-window-set-status");
  status = scm_to_locale_string  (text);
  if (w != 0)
  {
    int i;
    int len = strlen(status);
    glutSetWindow (w);
    glPushMatrix();
    
    // De-hardcode me!
    glColor3f(0.9, 0.8, 0.8);
    glRasterPos2f(5.2, 4.8);
    for (i = 0; i < len; i++)
      {
	// De-hardcode me!
	glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, status[i]);
      }
    // De-hardcode me!
    glColor3f(0.1, 0.1, 0.25);
    glRasterPos2f(5.0, 5.0);
    for (i = 0; i < len; i++)
      {
	// De-hardcode me!
	glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, status[i]);
      }
    glPopMatrix();
  }
  if (old_win != 0)
    glutSetWindow (old_win);
  free (status);
  return SCM_EOL;
}

/**
   Get the window ready for drawing.
   @param win The window id to get ready for drawing.
   @return '()
*/

SCM minara_window_draw_begin(SCM win)
{
  //This may change as the renderer evolves
  glShadeModel (GL_FLAT);
  //TODO:Anti - aliasing.
  // Allow enabling / disabling from Scheme / preferences
  // Disable costly functions
  // (most are disabled anyway)
  glDisable (GL_DITHER);
  glDisable (GL_DEPTH_TEST);
  glClearColor (1.0, 1.0, 1.0, 1.0);
  glClear (GL_COLOR_BUFFER_BIT);
  return SCM_EOL;
}

/**
   Finish drawing in the window.
   @param win The window id to finish drawing in.
   @return '()
*/

SCM minara_window_draw_end(SCM win)
{
  glFlush ();
  glutSwapBuffers ();
  return SCM_EOL;
}

//Program lifecycle

/**
   Register our Guile functions
*/

void
window_startup ()
{
  //Register our scheme functions
  scm_c_define_gsubr ("window-make", 2, 0, 0, minara_window_make);
  scm_c_define_gsubr ("window-dispose", 0, 0, 0, minara_window_dispose);
  scm_c_define_gsubr ("window-current-id", 0, 0, 0, minara_window_current);
  scm_c_define_gsubr ("window-set", 1, 0, 0, minara_window_set);
  scm_c_define_gsubr ("window-set-title", 2, 0, 0, minara_window_set_title);
  scm_c_define_gsubr ("window-draw-status", 2, 0, 0, minara_window_draw_status);
  scm_c_define_gsubr ("window-invalidate", 1, 0, 0, minara_window_invalidate);
  scm_c_define_gsubr ("window-draw-begin", 1, 0, 0, minara_window_draw_begin);
  scm_c_define_gsubr ("window-draw-end", 1, 0, 0, minara_window_draw_end);
}
