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
  TODO:   gsave/grestore
  transformations
*/

/*-----------------------------------------------------------------------------
  Includes
  ---------------------------------------------------------------------------*/

#include <stdlib.h>

#include <libguile.h>

#ifdef __APPLE__
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <GLUT/glut.h>
#else
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#endif

#include "minara_error.h"
#include "minara_guile.h"

#include "minara_rendering.h"

/*-----------------------------------------------------------------------------
  Local defines
  ---------------------------------------------------------------------------*/

#define kPointCacheSize (1024)
#define kPointCacheStride (3)

/*-----------------------------------------------------------------------------
  Local Structs
  ---------------------------------------------------------------------------*/

/** Our tesselator point cache.
    gluTessVertex doesn't access the vertex data until later, so we need to 
    make sure any data passed in is live when we finish the poly. So we cache 
    it in buffers of doubles in a linked list that can be quickly allocated 
    as needed, then quickly deallocated when finished with.*/
typedef struct PointCache {
  struct PointCache * next;
  GLdouble * current;
  GLdouble * end;
  GLdouble points[0];
} PointCache;

/*-----------------------------------------------------------------------------
  Globals
  ---------------------------------------------------------------------------*/

/** Have we started drawing a path? 
    If so, we need to close before we moveto, 
    if not we need to initialise before we moveto. */
static int gPathStarted = 0;

/** Our one true, we-aren't-threaded tesselator. */
static GLUtesselator * gTess = NULL;

/** Our one true, we-aren't-threaded point cache. */
static PointCache * gPointCache = NULL;

/** The previous point. Required for bezier rendering */
static GLdouble gPreviousPoint[2] = {0.0, 0.0};

/*-----------------------------------------------------------------------------
  Local Prototypes
  ---------------------------------------------------------------------------*/

static MErr PointCacheInitialise (PointCache ** root);
static void PointCacheFinalise (PointCache ** root);
static MErr PointCacheIncreaseCapacity (PointCache ** root);
static MErr PointCacheInsertPoint (PointCache ** root, GLdouble ** coords, GLdouble x, GLdouble y, GLdouble z);

/*-----------------------------------------------------------------------------
  Local Methods
  ---------------------------------------------------------------------------*/

//  The Point Cache

/**
   Make sure the cache is initialised.
   @param root A pointer to the root pointer for the cache, possibly null.
   @return A Minara error code, possibly out of memory.
*/
MErr PointCacheInitialise (PointCache ** root) {
  if (*root == NULL) {
    return_on_error(PointCacheIncreaseCapacity (root));
  }
  return kNoErr;
}

/**
   Increase the capacity of the cache.
   @param root A pointer to the root pointer for the cache, possibly null.
   @return A Minara error code, possibly out of memory.
*/

MErr PointCacheIncreaseCapacity (PointCache ** root) {
  // 3 Doubles to a float. We only use 2, though...
  int cacheSize = kPointCacheSize * kPointCacheStride * sizeof(GLdouble); 
  PointCache * p = (PointCache *)malloc (sizeof(PointCache) + cacheSize);
  if (p == NULL) {
    // return kOutOfMemoryErr;
  }
  p->current = p->points;
  p->end = p->current + (kPointCacheSize * kPointCacheStride);
  p->next = *root;
  *root = p;
  return kNoErr;
}

/**
   Finalise the cache, deallocating the storage.
   @param root A pointer to the root pointer for the cache, possibly null on input,
   definitely null on output.
*/

void PointCacheFinalise (PointCache ** root) {
  PointCache * c = *root;
  while (c != NULL) {
    PointCache * next = c->next;
    free (c);
    c = next;
  }
  *root = NULL;
}

/**
   Insert a point into the cache.
   @param root A pointer to the root pointer for the cache, possibly null.
   @param coords A pointer to a double pointer for the coordinate array in the cache.
   @param x The X co-ordinate.
   @param y The Y co-ordinate.
   @param z The Z co-ordinate.
   @return A Minara error code, possibly out of memory.
*/

MErr PointCacheInsertPoint (PointCache ** root, GLdouble ** coords, GLdouble x, GLdouble y, GLdouble z) {
  // Expand the capacity if required
  if ((*root)->current == (*root)->end) {
    return_on_error(PointCacheIncreaseCapacity (root));
  }
  // Copy the doubles in.
  // We only use the first 2, and we should be able to optimise the copy
  ((*root)->current)[0] = x;  
  ((*root)->current)[1] = y; 
  ((*root)->current)[2] = z;   
  // Pass back the entry
  *coords = (*root)->current;
  // And move on
  ((*root)->current) = ((*root)->current) + kPointCacheStride;
  return kNoErr;
}

//  Guile Functions

// Path Construction

/**
   Start a new path.
   @return Scheme nil.
*/

SCM render_path_begin () {
  PointCacheInitialise (&gPointCache);
  gluTessBeginPolygon (gTess, NULL);
  gPathStarted = 0;
  gPreviousPoint[0] = 0.0;
  gPreviousPoint[1] = 0.0;
  //fprintf (stderr, "render-path-begin\n");
  return SCM_EOL;
}

/**
   End a path, correctly ending subpaths.
   @return Scheme nil.
*/

SCM render_path_end () {
  if (gPathStarted == 1) {
    gluTessEndContour (gTess);
    gPathStarted = 0;
  }
  gluTessEndPolygon (gTess);
  //fprintf (stderr, "render-path-end\n");
  return SCM_EOL;
}

/**
   Start a new subpath.
   @param horizontal The Scheme x co-ordinate.
   @param vertical The Scheme y co-ordinate.
   @return Scheme nil.
*/

SCM render_move_to (SCM horizontal, SCM vertical) {
  double h, v;
  GLdouble * coords;
  SCM_ASSERT(SCM_NUMBERP(horizontal), horizontal, SCM_ARG1, "render-start-path");
  SCM_ASSERT(SCM_NUMBERP(vertical), vertical, SCM_ARG2, "render-start-path");
  h = scm_num2dbl (horizontal, "render-start-path");
  v = scm_num2dbl (vertical, "render-start-path");
  if (gPathStarted == 1) {
    gluTessEndContour (gTess);
  }
  gluTessBeginContour (gTess);
  PointCacheInsertPoint (&gPointCache, &coords, h, v, 0.0);
  gluTessVertex (gTess, coords, coords);
  gPathStarted = 1;
  gPreviousPoint[0] = h;
  gPreviousPoint[1] = v;
  //fprintf (stderr, "render-path-move-to %f %f\n", h, v);
  return SCM_EOL;
}

/**
   Add a line to a subpath.
   @param horizontal The Scheme x co-ordinate.
   @param vertical The Scheme y co-ordinate.
   @return Scheme nil.
*/

SCM render_line_to (SCM horizontal, SCM vertical) {
  double h, v;
  GLdouble * coords;
  SCM_ASSERT(SCM_NUMBERP(horizontal), horizontal, SCM_ARG1, "render-line-to");
  SCM_ASSERT(SCM_NUMBERP(vertical), vertical, SCM_ARG2, "render-line-to");
  h = scm_num2dbl (horizontal, "render-line-to");
  v = scm_num2dbl (vertical, "render-line-to");
  PointCacheInsertPoint (&gPointCache, &coords, h, v, 0.0);
  gluTessVertex (gTess, coords, coords);
  gPreviousPoint[0] = h;
  gPreviousPoint[1] = v;
  //fprintf (stderr, "render-path-line-to %f %f\n", h, v);
  return SCM_EOL;
}

/**
   Add a curve to a subpath.
   TODO: Fast and zoom-proof implementation.
   @param x1 The first Scheme x co-ordinate.
   @param y1 The first Scheme y co-ordinate.
   @param x2 The second Scheme x co-ordinate.
   @param y2 The second Scheme y co-ordinate.
   @param x3 The third Scheme x co-ordinate.
   @param y3 The third Scheme y co-ordinate.
   @return Scheme nil.
*/

#define kBezierSteps (12)

SCM render_curve_to (SCM x1, SCM y1, SCM x2, SCM y2, SCM x3, SCM y3) {
  double h1, v1, h2, v2, h3, v3;
  GLdouble *coords;
  double qx, qy;
  double q1, q2, q3, q4;
  double t = 0.0;
  double step = 1.0 / kBezierSteps;
  SCM_ASSERT(SCM_NUMBERP(x1), x1, SCM_ARG1, "render-curve-to");
  SCM_ASSERT(SCM_NUMBERP(y1), y1, SCM_ARG2, "render-curve-to");
  SCM_ASSERT(SCM_NUMBERP(x2), x2, SCM_ARG3, "render-curve-to");
  SCM_ASSERT(SCM_NUMBERP(y2), y2, SCM_ARG4, "render-curve-to");
  SCM_ASSERT(SCM_NUMBERP(x3), x3, SCM_ARG5, "render-curve-to");
  SCM_ASSERT(SCM_NUMBERP(y3), y3, SCM_ARG6, "render-curve-to");
  h1 = scm_num2dbl (x1, "render-curve-to");
  v1 = scm_num2dbl (y1, "render-curve-to");
  h2 = scm_num2dbl (x2, "render-curve-to");
  v2 = scm_num2dbl (y2, "render-curve-to");
  h3 = scm_num2dbl (x3, "render-curve-to");
  v3 = scm_num2dbl (y3, "render-curve-to");
  while (t <= 1.0) {
    q1 = t*t*t*-1.0 + t*t*3 + t*-3.0 + 1.0;
    q2 = t*t*t*3.0 + t*t*-6.0 + t*3.0;
    q3 = t*t*t*-3.0 + t*t*3.0;
    q4 = t*t*t;
    qx = q1*gPreviousPoint[0] + q2*h1 + q3*h2 + q4*h3;
    qy = q1*gPreviousPoint[1] + q2*v1 + q3*v2 + q4*v3;
    PointCacheInsertPoint (&gPointCache, &coords, qx, qy, 0.0);
    gluTessVertex (gTess, coords, coords);
    t = t + step;
  }
  PointCacheInsertPoint (&gPointCache, &coords, h3, v3, 0.0);
  gluTessVertex (gTess, coords, coords);
  gPreviousPoint[0] = h3;
  gPreviousPoint[1] = v3;
  //fprintf (stderr, "render-path-curve-to %f %f %f %f %f %f\n", h1, v1, h2, v2, h3, v3);
  return SCM_EOL;
}

//  Rendering

/**
   Set the colour that shapes drawn afterwards will be.
   @param r Scheme red.
   @param g Scheme green.
   @param b Scheme blue.
   @param a Scheme alpha.
   @return Scheme nil.
*/

SCM render_set_colour (SCM r, SCM g, SCM b, SCM a) {
  double af, rf, gf, bf;
  SCM_ASSERT(SCM_NUMBERP(r), r, SCM_ARG1, "render-fill");
  SCM_ASSERT(SCM_NUMBERP(g), g, SCM_ARG2, "render-fill");
  SCM_ASSERT(SCM_NUMBERP(b), b, SCM_ARG3, "render-fill");
  SCM_ASSERT(SCM_NUMBERP(a), a, SCM_ARG4, "render-fill");
  rf = scm_num2dbl (r, "render-fill");
  gf = scm_num2dbl (g, "render-fill");
  bf = scm_num2dbl (b, "render-fill");
  af = scm_num2dbl (a, "render-fill");
  glColor4f (rf, gf, bf, af);
  //fprintf (stderr, "render-set-color %f %f %f %f\n", rf, gf, bf, af);
  return SCM_EOL;
}

/** Redirect paths to the stencil buffer to generate the mask. */

SCM render_mask_begin() {
  glClear(GL_STENCIL_BUFFER_BIT);
  glStencilFunc(GL_ALWAYS, 0x1, 0x1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);
  return SCM_EOL;
}

/** Finish capturing the mask and start masking painting operations. */

SCM render_mask_end() {
  return SCM_EOL;
}

/** Start masking. */

SCM render_masking_begin() {
  glClearStencil(0x0);
  glEnable(GL_STENCIL_TEST);
  return SCM_EOL;
 }

/** Stop masking. */

SCM render_masking_end() {
  glDisable(GL_STENCIL_TEST);
  glClear(GL_STENCIL_BUFFER_BIT);
  return SCM_EOL;
}

//  GLU tesselation callbacks

/** The glu tesselation error callback.
    Hook into Scheme?
    Prints an error message then exits.
    @param err The error code.
*/

void TessErrorCallback (GLenum err) {
  const GLubyte *es = gluErrorString (err);
  fprintf (stderr, "Tesselation error: %s\n", es);
  exit (1);
}

//  Program lifecycle

/**
   Register the Guile methods for rendering, and set up our polygon tesselator.
*/

void DefineRenderingModule () {
  // Register our functions
  scm_c_define_gsubr ("path-begin", 0, 0, 0,  
		      render_path_begin);
  scm_c_define_gsubr ("path-end", 0, 0, 0,
		      render_path_end);
  scm_c_define_gsubr ("move-to", 2, 0, 0, render_move_to);
  scm_c_define_gsubr ("line-to", 2, 0, 0, render_line_to);
  scm_c_define_gsubr ("curve-to", 6, 0, 0, render_curve_to);
  scm_c_define_gsubr ("set-colour", 4, 0, 0, render_set_colour);
  scm_c_define_gsubr ("mask-begin", 0, 0, 0, render_mask_begin);
  scm_c_define_gsubr ("mask-end", 0, 0, 0, render_mask_end);
  scm_c_define_gsubr ("masking-begin", 0, 0, 0, render_masking_begin);
  scm_c_define_gsubr ("masking-end", 0, 0, 0, render_masking_end);
  // Export them
  scm_c_export ("path-begin", "path-end", "move-to",
		"line-to", "curve-to", "set-colour", 
		"mask-begin", "mask-end", "masking-begin", 
		"masking-end", NULL);
}

void RenderingStartup () {
  // Make our tesselator
  gTess = gluNewTess ();
  gluTessCallback (gTess, GLU_TESS_VERTEX, glVertex3dv);
  gluTessCallback (gTess, GLU_TESS_BEGIN, glBegin);
  gluTessCallback (gTess, GLU_TESS_END, glEnd);
  gluTessCallback (gTess, GLU_TESS_ERROR, TessErrorCallback);
  // Define our module
  scm_c_define_module ("rendering", DefineRenderingModule, NULL);
}
