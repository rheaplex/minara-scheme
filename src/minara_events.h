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

#ifndef MINARA_EVENTS_INCLUDE
#define MINARA_EVENTS_INCLUDE

#include <libguile.h>

// The event callbacks
extern SCM quit_hook;
extern SCM resize_hook;
extern SCM draw_hook;
extern SCM mouse_button_down_hook;
extern SCM mouse_button_up_hook;
extern SCM mouse_move_hook;
extern SCM key_press_hook;
extern SCM menu_select_hook;

void bind_event_hooks ();
void events_startup ();

void glut_display ();
void glut_resize (int width, int height);
void glut_key_press (unsigned char key, int x, int y);
void glut_mouse_button (int button, int state, int x, int y);
void glut_mouse_drag (int x, int y);
void glut_mouse_move (int x, int y);
void glut_menu_select (int id);

#endif
