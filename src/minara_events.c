/*
    minara - a programmable graphics program editor
    Copyright (C) 2004, 2009  Rob Myers rob@robmyers.org

    This file is part of minara.

    minara is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    minara is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with minara.  If not, see <http://www.gnu.org/licenses/>.
*/

/*-----------------------------------------------------------------------------
  Events
  
  These are our application framework event handler hooks.
  Our event handlers receive events from the window system, does some setup
  for the rendering system and then feed them into the scheme code.
  Currently these are GLUT event handlers, which should probably be the 
  conceptual model for them. We could have an event polling system or a class
  with overriden event handlers and it wouldn't make any difference, though.
  ---------------------------------------------------------------------------*/

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
SCM quit_hook;
/** The current window resize event scheme hook */
SCM resize_hook;
/** The current window expose event scheme hook */
SCM draw_hook;
/** The current mouse button event scheme hook */
SCM mouse_button_down_hook;
/** The current mouse button release event scheme hook */
SCM mouse_button_up_hook;
/** The current mouse moved event scheme hook */
SCM mouse_move_hook;
/** The current key pressed event scheme hook */
SCM key_press_hook;
/** The current key releaseded event scheme hook */
SCM key_release_hook;
/** The current menu selected event scheme hook */
SCM menu_select_hook;

/*-----------------------------------------------------------------------------
  Functions
  ---------------------------------------------------------------------------*/

//Callback and Scheme interfacing

/**
   (Re)bind the C hooks to the Scheme callbacks.
   If the Scheme code has changed these, the C bindings will point to the old
   versions, so this needs re-doing whenever the C or the Scheme changes the
   hooks.
*/

void
bind_event_hooks ()
{
  quit_hook = 
    scm_c_eval_string ("(@@ (minara events) %quit-hook)");
  resize_hook = 
    scm_c_eval_string ("(@@ (minara events) %resize-hook)");
  draw_hook = 
    scm_c_eval_string ("(@@ (minara events) %draw-hook)");
  mouse_button_down_hook = 
    scm_c_eval_string ("(@@ (minara events) %mouse-down-hook)");
  mouse_button_up_hook = 
    scm_c_eval_string ("(@@ (minara events) %mouse-up-hook)");
  mouse_move_hook = 
    scm_c_eval_string ("(@@ (minara events) %mouse-move-hook)");
  key_press_hook = 
    scm_c_eval_string ("(@@ (minara events) %key-press-hook)");
  key_release_hook = 
    scm_c_eval_string ("(@@ (minara events) %key-release-hook)");
  menu_select_hook = 
    scm_c_eval_string ("(@@ (minara events) %menu-select-hook)");
}

/**
   Tools call this to hook themselves into the system.
   So they set the relevent hook, eg %draw-hook, then call this.
   @return A SCM for guile, ignore.
*/

SCM
scm_bind_event_hooks ()
{
  bind_event_hooks ();
  return SCM_EOL;
}


//Event hook calling

/**
   The window redraw callback for GLUT.
*/

void
glut_display ()
{
  scm_call_1 (draw_hook, minara_window_current ());
}

/**
   The window resize callback for GLUT.
   @param width The new width of the window in pixels.
   @param height The new height of the window in pixels.
*/

void
glut_resize (int width, int height)
{
  //Reshape the OpenGL viewport
  glViewport (0, 0, (GLsizei) width, (GLsizei) height);
  glMatrixMode (GL_PROJECTION);
  glLoadIdentity ();
  gluOrtho2D (0.0, (GLdouble) width, 0.0, (GLdouble) height);
  glMatrixMode (GL_MODELVIEW);
  glLoadIdentity ();
  //Tell guile
    scm_call_3 (resize_hook, minara_window_current (),
		scm_long2num (width),
		scm_long2num (height));

}

/**
   The key press callback for GLUT.
   @param key The ascii key code.
   @param x The x co-ordinate the mouse is at.
   @param y The y co-ordinate the mouse is at.
*/

void
glut_key_press (unsigned char key, int x, int y)
{
  int modifiers = glutGetModifiers ();
  char keyString[] = {key, '\0'};
  scm_call_3 (key_press_hook, minara_window_current (),
	      scm_makfrom0str (keyString), scm_long2num (modifiers));
}

/**
   The key release callback for GLUT.
   @param key The ascii key code.
   @param x The x co-ordinate the mouse is at.
   @param y The y co-ordinate the mouse is at.
*/

void
glut_key_release (unsigned char key, int x, int y)
{
  int modifiers = glutGetModifiers ();
  char keyString[] = {key, '\0'};
  scm_call_3 (key_release_hook, minara_window_current (),
	      scm_makfrom0str (keyString), scm_long2num (modifiers));
}

/**
   The mouse button press callback for GLUT.
   @param button The button: left, middle or right.
   @param state Whether the button has been pressed or released.
   @param x The x co-ordinate the mouse is at.
   @param y The y co-ordinate the mouse is at.
*/

void
glut_mouse_button (int button, int state, int x, int y)
{
  int buttonNum;
  switch (button)
  {
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
  if (state == GLUT_UP)
  {
    scm_call_4 (mouse_button_up_hook, minara_window_current (),
		scm_long2num (buttonNum),
		scm_long2num (x), scm_long2num (y));
  }
  else
  {
    scm_call_4 (mouse_button_down_hook, minara_window_current (),
		scm_long2num (buttonNum),
		scm_long2num (x), scm_long2num (y));
  }
}

/**
   The mouse drag (move with buttons down) callback for GLUT.
   @param x The x co-ordinate the mouse is now at.
   @param y The y co-ordinate the mouse is now at.
*/

void
glut_mouse_drag (int x, int y)
{
  scm_call_3 (mouse_move_hook, minara_window_current (),
	      scm_long2num (x), scm_long2num (y));
}

/**
   The mouse motion callback for GLUT.
   @param x The x co-ordinate the mouse is now at.
   @param y The y co-ordinate the mouse is now at.
*/

void
glut_mouse_move (int x, int y)
{
  scm_call_3 (mouse_move_hook, minara_window_current (),
	      scm_long2num (x), scm_long2num (y));
}

/**
   The menu select callback for GLUT.
   @param id The menu id number.
*/

void
glut_menu_select (int id)
{
  scm_call_2 (menu_select_hook, minara_window_current (),
	      scm_long2num (id));
}

/**
   Register the Guile methods for events.
*/

void
define_events_module ()
{
  //Register our functions
  scm_c_define_gsubr ("%bind-event-hooks", 0, 0, 0, scm_bind_event_hooks);
  //Export them
  scm_c_export ("%bind-event-hooks", NULL);
}

/**
   Register the guile extensions used in managing the main event loop
*/

void
events_startup ()
{
  //Define our module
  scm_c_define_module ("minara-internal events", define_events_module, NULL);
}
