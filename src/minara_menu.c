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

/** The main (contextual) menu. */
static int gMainMenu;

/*-----------------------------------------------------------------------------
  Functions
  ---------------------------------------------------------------------------*/

// Scheme methods

/**
   Make a submenu of the main menu.
   @param title The name of the menu.
   @return The new menu ID .
*/

SCM minara_menu_make () {
  int idNum = -1;
  idNum = glutCreateMenu (GlutMenuSelect);
  glutSetMenu (gMainMenu);
  return SCM_MAKINUM(idNum);
}

/**
   Install the menu into the main menu
   @param menu The menu id to install
   @return '()
*/

SCM minara_menu_install (SCM menu, SCM title) {
  int menuNum;
  char * titleStr = "";
  SCM_ASSERT(SCM_STRINGP(title), title, SCM_ARG1, "minara-menu-install");
  SCM_STRING_COERCE_0TERMINATION_X(title);
  titleStr = SCM_STRING_CHARS (title);
  SCM_ASSERT(SCM_NUMBERP(menu), menu, SCM_ARG1, "minara-menu-install");
  menuNum = scm_num2int (menu, SCM_ARG1, "minara-menu-install");
  glutSetMenu(gMainMenu);
  glutAddSubMenu (titleStr, menuNum);
  return SCM_EOL;
}

/**
   Make and add a menu entry to a menu with a given id.
   @param menu The menu to add the new entry to.
   @param entry The string title of the menu.
   @param id The unique id number for the menu entry.
   @return '()
*/

SCM minara_menu_add_entry (SCM menu, SCM entry, SCM id) {
  int menuNum, idNum;
  char * entryStr;
  SCM_ASSERT(SCM_NUMBERP(menu), menu, SCM_ARG1, "minara-menu-add-entry");
  SCM_ASSERT(SCM_STRINGP(entry), entry, SCM_ARG2, "minara-menu-add-entry");
  SCM_ASSERT(SCM_NUMBERP(id), id, SCM_ARG3, "minara-menu-add-entry");
  menuNum = scm_num2int (menu, SCM_ARG1, "minara-menu-add-entry");
  SCM_STRING_COERCE_0TERMINATION_X(entry);
  entryStr = SCM_STRING_CHARS(entry);
  idNum = scm_num2int (id, SCM_ARG3, "minara-menu-add-entry");
  glutSetMenu (menuNum);
  glutAddMenuEntry (entryStr, idNum);
  glutSetMenu (gMainMenu);
  return SCM_EOL;
}

/**
   Remove a menu entry with a given id.
   @param id The unique id number of the item to remove.
   @return '()
*/

SCM minara_menu_remove_entry (SCM id) {
  int idNum;
  SCM_ASSERT(SCM_NUMBERP(id), id, SCM_ARG1, "minara-menu-remove-entry");
  idNum = scm_num2int (id, SCM_ARG1, "minara-menu-remove-entry");
  glutRemoveMenuItem (idNum);
  return SCM_EOL;
}

// Program lifecycle

/**
   Register our Guile functions
*/

void MenuStartup () {
  // Register our scheme functions
  scm_c_define_gsubr ("menu-make", 0, 0, 0, minara_menu_make);
  scm_c_define_gsubr ("menu-install", 2, 0, 0, minara_menu_install);
  scm_c_define_gsubr ("menu-add-entry", 3, 0, 0, minara_menu_add_entry);
  scm_c_define_gsubr ("menu-remove-entry", 1, 0, 0, minara_menu_remove_entry);
  
  // Make a main menu to attach everything to.
  gMainMenu = glutCreateMenu(NULL);
  //glutAttachMenu(GLUT_RIGHT_BUTTON);
}
