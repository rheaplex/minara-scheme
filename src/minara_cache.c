/*
    minara - a programmable graphics program editor
    Copyright (C) 2004  Rob Myers rob@robmyers.org

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
  Caches.
  
  A cache is a stored, optimised series of drawing instructions.
  Drawing a cache should be faster than interpreting the instructions again.
  A cache is currently an OpenGL display list, and this should probably be
  the conceptual model for it.
  ---------------------------------------------------------------------------*/
  
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
#include "minara_cache.h"

/*-----------------------------------------------------------------------------
  Global Variables
  ---------------------------------------------------------------------------*/


/*-----------------------------------------------------------------------------
  Functions
  ---------------------------------------------------------------------------*/

// Scheme functions

/**
   Make a graphics instruction cache.
   @return The cache ID or '()
*/

SCM
minara_cache_make ()
{
  GLuint c = glGenLists (1);
  return scm_uint2num (c);
}

/**
   Destroy a cache
   @param cache The cache to finalise and deallocate.
   @return '()
*/

SCM
minara_cache_dispose (SCM cache)
{
  GLuint c = 0;
  SCM_ASSERT (SCM_NUMBERP (cache), cache, SCM_ARG1, "minara-cache-dispose");
  c = (GLuint) scm_num2uint (cache, SCM_ARG1, "minara-cache-dispose");
  glDeleteLists (c, 1);
  return SCM_EOL;
}

/**
   Draw a cache
   @param cache The cache to draw.
   @return '()
*/

SCM
minara_cache_draw (SCM cache)
{
  GLuint c = 0;
  SCM_ASSERT (SCM_NUMBERP (cache), cache, SCM_ARG1, "minara-cache-draw");
  c = (GLuint) scm_num2uint (cache, SCM_ARG1, "minara-cache-draw");
  glCallList (c);
  glFlush ();
  return SCM_EOL;
}

/**
   Start recording a cache
   @param cache The cache to start recording.
   @return '()
*/

SCM
minara_cache_record_begin (SCM cache)
{
  GLuint c = 0;
  SCM_ASSERT (SCM_NUMBERP (cache), cache, SCM_ARG1, "minara-cache-record-begin");
  c = (GLuint) scm_num2uint (cache, SCM_ARG1, "minara-cache-record-begin");
  glNewList (c, GL_COMPILE_AND_EXECUTE);
  return SCM_EOL;
}

/**
   Finish recording a cache
   @param cache The cache to finish recording.
   @return '()
*/

SCM
minara_cache_record_end (SCM cache)
{
  glFlush ();
  glEndList ();
  return SCM_EOL;
}

//Program lifecycle

/**
   Register our Guile functions
*/

void
define_cache_module ()
{
  //Register our scheme functions
  scm_c_define_gsubr ("cache-make", 0, 0, 0, minara_cache_make);
  scm_c_define_gsubr ("cache-dispose", 1, 0, 0, minara_cache_dispose);
  scm_c_define_gsubr ("cache-draw", 1, 0, 0, minara_cache_draw);
  scm_c_define_gsubr ("cache-record-begin", 1, 0, 0,
		      minara_cache_record_begin);
  scm_c_define_gsubr ("cache-record-end", 1, 0, 0, minara_cache_record_end);

  scm_c_export ("cache-make", "cache-dispose", "cache-draw",
		"cache-record-begin", "cache-record-end", NULL);
}

void
cache_startup ()
{
  //Define our module
  scm_c_define_module ("minara-internal cache", define_cache_module, NULL);
}
