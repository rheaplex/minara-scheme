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

#ifndef MINARA_ERROR_INCLUDE
#define MINARA_ERROR_INCLUDE

// Error codes (1 = OK, <= 0 not)

typedef enum {
  kNoErr = 1,
  kErr = 0,
  kOutOfMemoryErr = -1,
  kStackWouldOverflowErr = -2,
  kStackWouldUnderflowErr = -3,
} MErr;

// Careful of this, it hides flow of execution

#define return_on_error(fun)						\
  do { MErr err = fun; if(err != kNoErr) return err; } while (0)

#endif
