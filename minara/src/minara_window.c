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

static int MinaraWindowGlutMake(char * name, int width, int height);
static void GlutWindowSet (int win);

/*-----------------------------------------------------------------------------
  Global Variables
  ---------------------------------------------------------------------------*/

// Get these from the launch environment
/** The window startup width */
int gScreenWidth = 640;
/** The window startup height */
int gScreenHeight = 480;

/**The windows*/
MinaraWindow * gWindows = NULL;

// Development code!!!

char * gDevDraw = /*
  "(%render-set-colour 1.0 0.0 0.0 1.0) \
         (%render-path-begin) \
         (%render-move-to 50.0 50.0) \
         (%render-line-to 200.0 50.0) \
         (%render-line-to 200.0 200.0) \
         (%render-line-to 50.0 200.0) \
	 (%render-path-end)(%render-set-colour 0.0 0.0 1.0 1.0) \
         (%render-path-begin) \
         (%render-move-to 50.0 50.0) \
         (%render-curve-to 50.0 200.0 200.0 200.0 200.0 50.0) \
         (%render-line-to 200.0 200.0) \
         (%render-line-to 50.0 200.0) \
	 (%render-path-end)"; */

"(%render-set-colour 0.0 0.828 0.387 1.0)\
(%render-path-begin)\
(%render-move-to 130.9441 79.3721)\
(%render-curve-to 125.2566 77.5049 131.3196 69.6743 125.5374 63.8911)\
(%render-curve-to 123.6707 62.0298 121.4358 61.0962 118.9163 61.0962)\
(%render-line-to 118.9163 79.7441)\
(%render-curve-to 118.9163 84.874 123.113 89.0703 128.2424 89.0703)\
(%render-line-to 137.3792 89.0703)\
(%render-curve-to 142.509 89.0703 146.7053 84.874 146.7053 79.7441)\
(%render-line-to 146.7053 61.0962)\
(%render-curve-to 144.1897 61.0962 141.9509 62.0298 140.179 63.8013)\
(%render-curve-to 134.302 69.6743 140.365 77.5049 134.6775 79.3721)\
(%render-line-to 130.9441 79.3721)\
\
(%render-move-to 243.6045 70.4224)\
(%render-curve-to 243.6045 65.2915 239.4082 61.0962 234.2783 61.0962)\
(%render-line-to 224.9521 61.0962)\
(%render-curve-to 219.8232 61.0962 215.6299 65.2915 215.6299 70.4224)\
(%render-line-to 215.6299 79.7441)\
(%render-curve-to 215.6299 84.874 219.8232 89.0703 224.9521 89.0703)\
(%render-line-to 243.6045 89.0703)\
(%render-curve-to 248.7344 89.0703 252.9307 84.874 252.9307 79.7441)\
(%render-line-to 252.9307 61.0962)\
(%render-curve-to 247.8008 61.0962 243.6045 65.2915 243.6045 70.4224)\
\
(%render-move-to 243.2334 77.042)\
(%render-curve-to 242.5801 79.0908 240.9941 79.7441 238.9414 79.7441)\
(%render-line-to 224.9521 79.7441)\
(%render-curve-to 224.9521 78.3477 224.9521 74.3364 225.3242 73.1235)\
(%render-curve-to 225.9775 71.0747 227.5635 70.4224 229.6152 70.4224)\
(%render-line-to 243.6045 70.4224)\
(%render-curve-to 243.6045 71.8169 243.6045 75.8281 243.2334 77.042)\
 \
(%render-move-to 207.3291 79.7441)\
(%render-line-to 202.8584 79.7441)\
(%render-curve-to 200.8096 79.7441 199.2236 79.0908 198.5713 77.042)\
(%render-curve-to 198.2002 75.8281 198.2002 71.7271 198.2002 70.4224)\
(%render-curve-to 198.2002 67.9028 197.2668 65.6646 195.4983 63.8911)\
(%render-curve-to 193.6321 62.0298 191.3938 61.0962 188.8743 61.0962)\
(%render-line-to 188.8743 79.7441)\
(%render-curve-to 188.8743 84.874 193.0696 89.0703 198.2002 89.0703)\
(%render-line-to 216.6543 89.0703)\
(%render-curve-to 216.6543 83.9404 212.458 79.7441 207.3291 79.7441)\
 \
(%render-move-to 94.4856 89.0703)\
(%render-line-to 66.5149 89.0703)\
(%render-curve-to 61.385 89.0703 57.1887 84.874 57.1887 79.7441)\
(%render-line-to 57.1887 61.0962)\
(%render-curve-to 59.7083 61.0962 61.947 62.0298 63.8137 63.8911)\
(%render-curve-to 69.5959 69.6743 63.533 77.5049 69.2166 79.3721)\
(%render-curve-to 70.3396 79.7441 73.8831 79.7441 75.0974 79.7441)\
(%render-line-to 75.6516 79.7441)\
(%render-line-to 75.6516 61.0962)\
(%render-curve-to 78.1707 61.0962 80.4094 62.0298 82.2761 63.8911)\
(%render-curve-to 84.0442 65.6646 84.9778 67.9028 84.9778 70.4224)\
(%render-curve-to 84.9778 71.7271 84.9778 75.8281 85.3494 77.042)\
(%render-line-to 85.2659 79.7441)\
(%render-line-to 85.9109 79.7441)\
(%render-curve-to 87.1252 79.7441 90.6687 79.7441 91.7844 79.3721)\
(%render-curve-to 97.4758 77.5049 91.4124 69.6743 97.2859 63.8013)\
(%render-curve-to 99.0618 62.0298 101.3 61.0962 103.8118 61.0962)\
(%render-line-to 103.8118 79.7441)\
(%render-curve-to 103.8118 84.874 99.6155 89.0703 94.4856 89.0703)\
 \
(%render-move-to 69.1482 104.2012)\
(%render-curve-to 63.2747 98.3242 55.4436 104.3867 53.5769 98.6992)\
(%render-curve-to 53.2053 97.5801 53.2053 94.0361 53.2053 92.8223)\
(%render-line-to 53.2053 58.6099)\
(%render-curve-to 53.2053 57.397 53.2053 53.8521 53.5769 52.7329)\
(%render-curve-to 55.4436 47.0454 63.2747 53.1079 69.0569 47.3267)\
(%render-curve-to 70.9202 45.4595 71.8538 43.2251 71.8538 40.7056)\
(%render-line-to 53.2053 40.7056)\
(%render-curve-to 48.0754 40.7056 43.8792 44.9028 43.8792 50.0317)\
(%render-line-to 43.8792 101.4014)\
(%render-curve-to 43.8792 106.5313 48.0754 110.7271 53.2053 110.7271)\
(%render-line-to 71.8538 110.7271)\
(%render-curve-to 71.8538 108.2114 70.9202 105.9727 69.1482 104.2012)\
 \
(%render-move-to 177.0208 70.4224)\
(%render-curve-to 177.0208 65.2915 172.8235 61.0962 167.6946 61.0962)\
(%render-line-to 158.3684 61.0962)\
(%render-curve-to 153.2385 61.0962 149.0461 65.2915 149.0461 70.4224)\
(%render-line-to 149.0461 79.7441)\
(%render-curve-to 149.0461 84.874 153.2385 89.0703 158.3684 89.0703)\
(%render-line-to 177.0208 89.0703)\
(%render-curve-to 182.1497 89.0703 186.3469 84.874 186.3469 79.7441)\
(%render-line-to 186.3469 61.0962)\
(%render-curve-to 181.217 61.0962 177.0208 65.2915 177.0208 70.4224)\
 \
(%render-move-to 176.6487 77.042)\
(%render-curve-to 175.9963 79.0908 174.4104 79.7441 172.3577 79.7441)\
(%render-line-to 158.3684 79.7441)\
(%render-curve-to 158.3684 78.3477 158.3684 74.3364 158.7395 73.1235)\
(%render-curve-to 159.3928 71.0747 160.9788 70.4224 163.0315 70.4224)\
(%render-line-to 177.0208 70.4224)\
(%render-curve-to 177.0208 71.8169 177.0208 75.8281 176.6487 77.042)\
 \
(%render-move-to 241.2256 47.231)\
(%render-curve-to 247.0986 53.1079 254.9307 47.0454 256.7969 52.7329)\
(%render-curve-to 257.1689 53.8521 257.1689 57.397 257.1689 58.6099)\
(%render-line-to 257.1689 92.8223)\
(%render-curve-to 257.1689 94.0361 257.1689 97.5801 256.7969 98.6992)\
(%render-curve-to 254.9307 104.3867 247.0986 98.3242 241.3174 104.1064)\
(%render-curve-to 239.4541 105.9727 238.5205 108.2085 238.5205 110.7271)\
(%render-line-to 257.1689 110.7271)\
(%render-curve-to 262.2979 110.7271 266.4951 106.5313 266.4951 101.4014)\
(%render-line-to 266.4951 50.0317)\
(%render-curve-to 266.4951 44.9028 262.2979 40.7056 257.1689 40.7056)\
(%render-line-to 238.5205 40.7056)\
(%render-curve-to 238.5205 43.2212 239.4541 45.4595 241.2256 47.231)\
 \
(%render-move-to 116.0027 70.4224)\
(%render-curve-to 116.0027 67.9028 115.0691 65.6646 113.301 63.8911)\
(%render-curve-to 111.4343 62.0298 109.1956 61.0962 106.6765 61.0962)\
(%render-line-to 106.6765 79.7441)\
(%render-curve-to 106.6765 84.874 110.8728 89.0703 116.0027 89.0703)\
(%render-line-to 116.0027 70.4224)\
(%render-path-end)";



         
/*-----------------------------------------------------------------------------
  Functions
  ---------------------------------------------------------------------------*/

// Window list management

/**
   Insert a newly allocated window into the list.
   @param root A pointer to the root pointer of the list.
   @param win The window id of the window.
   @param pixmap The Pixmap for the window
   @param path The path for the window.
*/

void MinaraWindowInsert (MinaraWindow ** root, MinaraWindow * con) {
  if (con != NULL) {
    con->next = *root;
    *root = con;
  }
}

/**
   Delete the window, but *NOT* the contained pointers.
   @param root A pointer to the root pointer of the list.
   @param win The window id to match.
*/

void MinaraWindowRemove (MinaraWindow ** root, int win) {
  // Empty list? Return
  if (*root == NULL) {
    return;
    // Deleting the first item? Replace and return
    // If next is NULL that's OK, it just sets the list to empty
  } else if ((*root)->window == win) {
    MinaraWindow * temp = *root;
    *root = (*root)->next;
    free (temp);
    return;
  } else {
    // Iterate through until we run out of items or find a match
    MinaraWindow * before = *root;
    MinaraWindow * current = before->next;
    // When current == null we've exhausted the list without a match
    while (current != NULL) {
      // Found a match?
      if (current->window == win) {
	// Remove the link and free the item
	before->next = current->next;
	free (current);
	break;
	// Otherwise
      } else {
	// Advance to the next node
	before = current;
	current = current->next;
      }
    }
  }
}

/**
   Get the window, returns NULL on failure.
   @param root The root pointer of the list.
   @param win The window id to match.
   @return A pointer to the window for the window id, or NULL if no match.
*/

MinaraWindow * MinaraWindowGet (MinaraWindow * root, int win) {
  MinaraWindow * con = root;
  while (con != NULL) {
    if (con->window == win) {
      break;
    } else {
      con = con->next;
    }
  }
  return con;
}

/**
   Allocate a window structure and insert it into the list
   @param con A pointer to the list root window pointer. *con may be NULL.
   @param width The window width.
   @param height The window height.
   @param name The window name.
*/

void MinaraWindowMake (MinaraWindow ** con, int width, int height, char * name) {
  // Make the window
  MinaraWindow * win = (MinaraWindow*)malloc (sizeof (MinaraWindow));
  if (win != NULL) {
    win->window = MinaraWindowGlutMake (name, width, height);
    win->displayList = glGenLists(1);
    win->buffer = gDevDraw;
    win->shouldRedraw = 1;
    win->next = NULL;
  }
  *con = win;
}

/**
   Deallocate a window structure, which must have been removed from the list.
   @param con A pointer to the window structure.
   @param width The window width.
   @param height The window height.
   @param name The window name.
*/

void MinaraWindowDestroy (MinaraWindow * con) {
  if (con != NULL) {
    int win = con->window;
    int oldWin = glutGetWindow ();
    GlutWindowSet (win);
    glutHideWindow ();
    GlutWindowSet (oldWin);
    glutDestroyWindow (win);
    glDeleteLists (con->displayList, 1);
    con->displayList = 0;
    // Deallocate the buffer properly!
    con->buffer = NULL;
    con->window = -1;
    con->next = NULL;
  }
}

/**
   Allocate a window structure and insert it into the list
   @param con The list root window pointer.
   @param width The new window width.
   @param height The new window height.
*/

void MinaraWindowResize (MinaraWindow * root, int win, 
			 int width, int height) {
  MinaraWindow * con = MinaraWindowGet (root, win);
  if (con != NULL) {
    con->shouldRedraw = 1;
  }
}

/*
  Make a new glut window and set up the data structures and callbacks.
  @param name The title to diplay on the window title bar.
  @return The ID of the created window.
*/

int MinaraWindowGlutMake(char * name, int width, int height) {
  int win;
  int oldWin;
  oldWin = glutGetWindow ();
  win = glutCreateWindow (name);
  GlutWindowSet (win);
  glutInitWindowSize (width, height); 
  glutInitWindowPosition (0, 0); 
  glutInitDisplayMode (GLUT_RGBA | GLUT_SINGLE);

  glClearColor (1.0, 1.0, 1.0, 1.0);
  // This may change as the renderer evolves
  glShadeModel (GL_FLAT);
  
  //TODO: Anti-aliasing. Allow enabling/disabling from Scheme/preferences
  
  // Disable costly functions
  //    (most are disabled anyway)
  glDisable (GL_DITHER);
  glDisable (GL_DEPTH_TEST);

  glutDisplayFunc (GlutDisplay);
  glutReshapeFunc (GlutResize);
  glutKeyboardFunc (GlutKeyPress);
  glutMouseFunc (GlutMouseButton);
  glutMotionFunc (GlutMouseDrag);
  glutPassiveMotionFunc (GlutMouseMove);

  GlutWindowSet (oldWin);
  
  return win;
}

/**
   Redraw the window graphics by evaluating the window script buffer in the rendering context.
   Either:
   Evaluate the script buffer for the window in the rendering context, capturing the output
   in an OpenGL display list.
   Or:
   Draw the display list.
   If we can't allocate the display list, we just re-evaluate.
   @param root The window list root pointer.
   @param win The window id to redraw.
*/

void MinaraWindowDraw (MinaraWindow * root, int win) {
  MinaraWindow * con = MinaraWindowGet (root, win);
  if (con != NULL) {
    GLuint list = con->displayList;
    char * buffer = con->buffer;
    // If we don't need to redraw and we have a display list, just draw it
    if ((con->shouldRedraw == 0) && (list != 0)) {
      glCallList (list);
      glFlush ();
    } else {
      // Otherwise, regenerate the list and redraw
      // Or, if we don't have a list, just redraw
      if (list != 0) {
	glNewList (list, GL_COMPILE_AND_EXECUTE);
      }
      //FIXME: Make an environ nested in render environ, get/save/set/reset values...
      //FIXME: Much error handling!!!
      // Clear the buffer
      glClear (GL_COLOR_BUFFER_BIT);
      // Evaluate the buffer to draw the graphics
      scm_c_eval_string (buffer);
      // make sure we finish drawing (not captured by display lists!)
      glFlush ();
      if (list != 0) {
        glEndList ();
      }
      con->shouldRedraw = 0;
    }
  }
}

/*
  Set the window, allowing for being passed an invalid window.
  @param win The window id. If zero, nothing will be done.
*/

static void GlutWindowSet (int win) {
  if (win != 0) {
    glutSetWindow (win);
  }
}

// Program lifecycle

/**
   Register any callbacks, allocate any globals.
*/

void WindowStartup () {
  // Register menu items and/or scheme hooks
}
