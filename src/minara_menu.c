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
  Menus
  
  Lisp functions to allow tools to add, configure and remove menus.
  
  We use GLUT contextual menus. This is terrible. We need real menus. Nothing
  should be done to prevent us using real menus one day.
  ---------------------------------------------------------------------------*/

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
static int32_t main_menu;

/*-----------------------------------------------------------------------------
  Functions
  ---------------------------------------------------------------------------*/

//Scheme methods

/**
   Make a submenu of the main menu.
   @param title The name of the menu.
   @return The new menu ID .
*/

SCM minara_menu_make ()
{
  int32_t id_num = -1;
  id_num = glutCreateMenu (glut_menu_select);
  glutSetMenu (main_menu);
  return scm_from_int (id_num);
}

/**
   Install the menu into the main menu
   @param menu The menu id to install
   @return '()
*/

SCM
minara_menu_install (SCM menu, SCM title)
{
  int32_t menu_num;
  char *title_str = "";
  SCM_ASSERT (scm_is_string (title), title, SCM_ARG1, "minara-menu-install");
  title_str = scm_to_locale_string (title);
  SCM_ASSERT (SCM_NUMBERP (menu), menu, SCM_ARG1, "minara-menu-install");
  menu_num = scm_to_int32 (menu);
  glutSetMenu (main_menu);
  glutAddSubMenu (title_str, menu_num);
  free (title_str);
  return SCM_EOL;
}

/**
   Make and add a menu entry to a menu with a given id.
   @param menu The menu to add the new entry to.
   @param entry The string title of the menu.
   @param id The unique id number for the menu entry.
   @return '()
*/

SCM
minara_menu_add_entry (SCM menu, SCM entry, SCM id)
{
  int32_t menu_num, id_num;
  char *entry_str;
  SCM_ASSERT (SCM_NUMBERP (menu), menu, SCM_ARG1, "minara-menu-add-entry");
  SCM_ASSERT (scm_is_string (entry), entry, SCM_ARG2, "minara-menu-add-entry");
  SCM_ASSERT (SCM_NUMBERP (id), id, SCM_ARG3, "minara-menu-add-entry");
  menu_num = scm_to_int32 (menu);
  entry_str = scm_to_locale_string (entry);
  id_num = scm_to_int32 (id);
  glutSetMenu (menu_num);
  glutAddMenuEntry (entry_str, id_num);
  glutSetMenu (main_menu);
  free (entry_str);
  return SCM_EOL;
}

/**
   Remove a menu entry with a given id.
   @param id The unique id number of the item to remove.
   @return '()
*/

SCM
minara_menu_remove_entry (SCM id)
{
  int32_t id_num;
  SCM_ASSERT (SCM_NUMBERP (id), id, SCM_ARG1, "minara-menu-remove-entry");
  id_num = scm_to_int32 (id);
  glutRemoveMenuItem (id_num);
  return SCM_EOL;
}

//Program lifecycle

/**
   Register our Guile functions
*/

void
define_menu_module ()
{
  //Register our scheme functions
  scm_c_define_gsubr ("menu-make", 0, 0, 0, minara_menu_make);
  scm_c_define_gsubr ("menu-install", 2, 0, 0, minara_menu_install);
  scm_c_define_gsubr ("menu-add-entry", 3, 0, 0, minara_menu_add_entry);
  scm_c_define_gsubr ("menu-remove-entry", 1, 0, 0, minara_menu_remove_entry);

  scm_c_export ("menu-make", "menu-install", "menu-add-entry",
                "menu-remove-entry", NULL);
}


void
menu_startup ()
{
  //Define our module
  scm_c_define_module ("minara-internal menu", define_menu_module, NULL);

  //Make a main menu to attach everything to.
  //main_menu = glutCreateMenu (NULL);
  //glutAttachMenu (GLUT_RIGHT_BUTTON);
}
