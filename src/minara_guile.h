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

#ifndef MINARA_GUILE_INCLUDE
#define MINARA_GUILE_INCLUDE

#include <stdio.h>
#include <stdlib.h>

#include <libguile.h>

#include "minara_error.h"

// Misnamed. Return a SCM error if the function returns a MErr

#define guile_error_return(x)				\
  do { MErr status = x;					\
    if (status != kNoErr) {				\
      fprintf (stderr, "Minara Error: %i\n", status);	\
      return SCM_BOOL_F;				\
    }							\
  } while (0)

void GuileStartup ();

#endif