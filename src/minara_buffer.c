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
  Undo operations are returned by functions that can generate them.
  These should be grouped into lists for each invocation of a tool unless
  the tool wishes to take control of them.
  NOTE: redo is storing the initially created funcall!

  TODO: Buffer from string & set from string (get strlen and (re)create)

  HELP!: Regexes...

  '(result-value undo)
*/

/*-----------------------------------------------------------------------------
  Includes  
  ---------------------------------------------------------------------------*/

#include <stdio.h>

#include <libguile.h>

#include "text.h"

#include "minara_buffer.h"

/*-----------------------------------------------------------------------------
  Local Prototypes
  ---------------------------------------------------------------------------*/

static SCM UndoBufferInsert (SCM buffer, SCM str, SCM position);
static SCM UndoBufferDelete (SCM buffer, Text * text, 
			     int position, int length);
static SCM UndoBufferReplace (SCM buffer, SCM str, SCM replace);
static SCM UndoBufferReplaceRange (SCM buffer, Text * text, SCM position, 
				   SCM count);
static SCM UndoBufferReplaceAll (SCM buffer, SCM str, SCM replace);

/*-----------------------------------------------------------------------------
  Global Variables
  ---------------------------------------------------------------------------*/

/** The buffer insert function, for creating undos. */
static SCM gBufferInsert;

/** The buffer delete function, for creating undos. */
static SCM gBufferDelete;

/** The buffer replace function, for creating undos. */
static SCM gBufferReplace;

/** The buffer range replace function, for creating undos. */
static SCM gBufferReplaceRange;

/** The buffer replace all function, for creating undos. */
static SCM gBufferReplaceAll;

/** The Scheme buffer type flag */

static scm_t_bits minara_buffer_tag;

/*-----------------------------------------------------------------------------
  Functions
  ---------------------------------------------------------------------------*/

// Undo makers

/**
   Make a lambda that will undo a buffer insert (eg delete it)
   @param buffer The buffer to operate on.
   @param str The string to analyse to find the length of deletion.
   @param position The position to delete at.
   @return The lambda.
*/

static SCM UndoBufferInsert (SCM buffer, SCM str, SCM position) {
  SCM undo;
  undo = SCM_LIST4(gBufferDelete, buffer, position, 
		   scm_long2num(SCM_STRING_LENGTH(str)));
  return undo;
}

/**
   Make a lambda that will undo a buffer delete (eg re-insert it)
   @param buffer The buffer to operate on.
   @param Text The text of the buffer (to save dereferencing).
   @param position The position to insert at.
   @param length The length of substring to insert.
   @return The lambda.
*/

static SCM UndoBufferDelete (SCM buffer, Text * text, int position, int length) {
  SCM undo;
  SCM str;
  // Make sure this COPIES the string! 
  // See strings.h, failing that try scm_mem2string
  // Make sure this GC protects the string (will SCM_LIST* GC?)
  str = scm_take_str (&(t_getall (text)[position]), length);
  undo = SCM_LIST4 (gBufferInsert, buffer, str, scm_int2num(position));
  return undo;
}

/**
   Make a lambda that will undo a buffer replace (eg reverse it)
   @param buffer The buffer to operate on.
   @param str The string to replace.
   @param replace The string to replace it with.
   @param position The position to replace the first occurrence after.
   @return The lambda.
*/

static SCM UndoBufferReplace (SCM buffer, SCM str, SCM replace) {
  SCM undo;
  undo = SCM_LIST4(gBufferReplace, buffer, replace, str);
  return undo;
}

static SCM UndoBufferReplaceRange (SCM buffer, Text * text, SCM position, 
				   SCM count) {
  SCM undo;
  SCM str;
  // Make sure this COPIES the string! 
  // See strings.h, failing that try scm_mem2string
  // Make sure this GC protects the string (will SCM_LIST* GC?)
  str = scm_take_str (&(t_getall (text)[SCM_INUM(position)]), SCM_INUM(count));
  undo = SCM_LIST5(gBufferReplaceRange, buffer, str, position, count);
  return undo;
}

/**
   Make a lambda that will undo a buffer replace all (eg reverse them)
   @param buffer The buffer to operate on.
   @param str The string to replace all occurrences of.
   @param replace The string to replace it with.
   @return The lambda.
*/

static SCM UndoBufferReplaceAll (SCM buffer, SCM str, SCM replace) {
  SCM undo;
  undo = SCM_LIST4(gBufferReplaceAll, buffer, replace, str);
  return undo;
}

// Buffer lifecycle

/**
   Initialise a buffer.
   @param buf The buffer to initialise.
*/

void MinaraBufferInitialise (MinaraBuffer * buf) {
  buf->changed = 0;
  buf->text = t_new ();
}

/**
   Finalise a buffer.
   @param buf The buffer to finalise.
*/

void MinaraBufferFinalise (MinaraBuffer * buf) {
  buf->changed = 0;
  t_free (buf->text);
}

// Scheme functions

/**
   Make a buffer
   @return The buffer ID or '()
*/

SCM minara_buffer_make () {
  MinaraBuffer * b = 
    (MinaraBuffer*)scm_must_malloc (sizeof(MinaraBuffer), "minara-buffer");
  MinaraBufferInitialise (b);
  SCM_RETURN_NEWSMOB (minara_buffer_tag, b);
}

/**
   Destroy a buffer
   @param buf The buffer to finalise and deallocate.
   @return The amount of memory deallocated
*/

size_t minara_buffer_destroy (SCM buffer) {
  MinaraBuffer * b = NULL;
  size_t size = 0;
  SCM_ASSERT (SCM_SMOB_PREDICATE (minara_buffer_tag, buffer),
              buffer, SCM_ARG1, "minara-buffer-destroy");
  b = (MinaraBuffer*) SCM_SMOB_DATA (buffer);
  size = sizeof(MinaraBuffer) + sizeof(Text) + b->text->size;
  MinaraBufferFinalise (b);
  scm_must_free (b);
  return size;
}

/**
   Load a buffer from file
   @param buf The buffer to load into, erasing the current contents.
   @param filepath The path of the file to load.
   @return '() or an error.
*/

SCM minara_buffer_load (SCM buffer, SCM filepath) {
  MinaraBuffer * b = NULL;
  char * path = "";
  SCM_ASSERT (SCM_SMOB_PREDICATE (minara_buffer_tag, buffer),
              buffer, SCM_ARG1, "minara-buffer-load");
  SCM_ASSERT(SCM_STRINGP(filepath), filepath, SCM_ARG2, "minara-buffer-load");
  b = (MinaraBuffer*) SCM_SMOB_DATA (buffer);
  SCM_STRING_COERCE_0TERMINATION_X (filepath);
  path = SCM_STRING_CHARS(filepath); 
  if (path == NULL) {
    return SCM_BOOL_T;
  }
  FILE * file = fopen (path, "r");
  if (file != NULL) {
    int fileSize;
    int readSize;
    // Get the file size and then restore the file position mark
    fileSize = fseek (file, 0, SEEK_END);
    fseek (file, 0, SEEK_SET);
    // Resize the buffer (and empty it)
    if (t_len(b->text) < fileSize) {
      Text * t = t_newdef( fileSize, 0, 1, T_ERR_BIN | T_ERR_MEM, NULL );
      if (t == NULL) {
	return SCM_BOOL_T;
      }
      t_free (b->text);
      b->text = t;
    }
    t_zero (b->text);
    // Read in the file
    readSize = fread (t_getall(b->text), 1, fileSize, file);
    fclose (file);
    b->text->len = readSize;
    // Make sure that we have read it all
    if (readSize != fileSize) {
      return SCM_BOOL_T;
    }
    b->changed = 1;
  } else {
    return SCM_BOOL_T;
  }
  return SCM_EOL;
}

/**
   Save a buffer to file
   @param buf The buffer to save from.
   @param filepath The path of the file to save to.
   @return '() or an error.
*/

SCM minara_buffer_save (SCM buffer, SCM filepath) {
  MinaraBuffer * b = NULL;
  char * path = "";
  SCM_ASSERT (SCM_SMOB_PREDICATE (minara_buffer_tag, buffer),
              buffer, SCM_ARG1, "minara-buffer-save");
  SCM_ASSERT(SCM_STRINGP(filepath), filepath, SCM_ARG2, "minara-buffer-save");
  b = (MinaraBuffer*) SCM_SMOB_DATA (buffer);
  SCM_STRING_COERCE_0TERMINATION_X (filepath);
  path = SCM_STRING_CHARS(filepath); 
  if (path == NULL) {
    return SCM_BOOL_T;
  }
  FILE * file = fopen (path, "w");
  if (file != NULL) {
    int writeSize = -1;
    // Write the file
    writeSize = fwrite (t_getall(b->text), 1, t_len(b->text), file);
    fclose (file);
    // Make sure we wrote it all
    if (writeSize != t_len(b->text)) {
      return SCM_BOOL_T;
    }
    b->changed = 0;
  } else {
    return SCM_BOOL_T;
  }
  return SCM_EOL;
}

/**
   Insert at the end of a buffer
   @param buf The buffer to append to.
   @param str The string to insert.
   @return The undo action for the append or '().
*/

SCM minara_buffer_append (SCM buffer, SCM str) {
  MinaraBuffer * b = NULL;
  char * cstr = "";
  SCM result = SCM_EOL;
  SCM_ASSERT (SCM_SMOB_PREDICATE (minara_buffer_tag, buffer),
              buffer, SCM_ARG1, "minara-buffer-append");
  SCM_ASSERT(SCM_STRINGP(str), str, SCM_ARG2, "minara-buffer-append");
  b = (MinaraBuffer*) SCM_SMOB_DATA (buffer);
  SCM_STRING_COERCE_0TERMINATION_X (str);
  cstr = SCM_STRING_CHARS(str); 
  if (cstr == NULL) {
    return SCM_BOOL_T;
  }
  // Make the undo action
  result = UndoBufferInsert (buffer, str, scm_long2num(b->text->max));
  // Perform the append
  t_apps (b->text, cstr);
  b->changed = 1;
  return result;
}

/**
   Insert at the beginning of a buffer
   @param buf The buffer to prepend to.
   @param str The string to insert.
   @return The undo action for the append or '().
*/

SCM minara_buffer_prepend (SCM buffer, SCM str) {
  MinaraBuffer * b = NULL;
  char * cstr = "";
  SCM result = SCM_EOL;
  SCM_ASSERT (SCM_SMOB_PREDICATE (minara_buffer_tag, buffer),
              buffer, SCM_ARG1, "minara-buffer-prepend");
  SCM_ASSERT(SCM_STRINGP(str), str, SCM_ARG2, "minara-buffer-prepend");
  b = (MinaraBuffer*) SCM_SMOB_DATA (buffer);
  SCM_STRING_COERCE_0TERMINATION_X (str);
  cstr = SCM_STRING_CHARS(str); 
  if (cstr == NULL) {
    return SCM_BOOL_T;
  }
  // Make the undo action
  result = UndoBufferInsert (buffer, str, 0);
  // Perform the prepend
  t_pres (b->text, cstr);
  b->changed = 1;
  return result;
}

/**
   delete within a buffer
   @param buf The buffer to prepend to.
   @param start The index to start at.
   @param count The number of characters to delete.
   @return The undo action for the append or '().
*/

SCM minara_buffer_delete (SCM buffer, SCM start, SCM count) {
  MinaraBuffer * b = NULL;
  unsigned int s = 0;
  unsigned int c = 0;
  SCM result = SCM_EOL;
  SCM_ASSERT (SCM_SMOB_PREDICATE (minara_buffer_tag, buffer),
              buffer, SCM_ARG1, "minara-buffer-delete");
  SCM_ASSERT(SCM_NUMBERP(start), start, SCM_ARG2, "minara-buffer-delete");
  b = (MinaraBuffer*) SCM_SMOB_DATA (buffer);
  s = scm_num2ulong (start, SCM_ARG2, "minara-buffer-delete");
  c = scm_num2ulong (count, SCM_ARG3, "minara-buffer-delete");
  // Make the undo action
  result = UndoBufferDelete (buffer, b->text, s, c);
  // Perform the delete
  t_delcposn (b->text, s, c);
  b->changed = 1;
  return result;
}

/**
   Insert within a buffer
   @param buf The buffer to prepend to.
   @param str The string to insert.
   @return The undo action for the append or '().
*/

SCM minara_buffer_insert (SCM buffer, SCM str, SCM position) {
  MinaraBuffer * b = NULL;
  SCM result;
  char * cstr = "";
  unsigned int pos = 0;
  SCM_ASSERT (SCM_SMOB_PREDICATE (minara_buffer_tag, buffer),
              buffer, SCM_ARG1, "minara-buffer-insert");
  SCM_ASSERT(SCM_STRINGP(str), str, SCM_ARG2, "minara-buffer-insert");
  SCM_ASSERT(SCM_NUMBERP(position), position, SCM_ARG3, 
	     "minara-buffer-insert");
  b = (MinaraBuffer*) SCM_SMOB_DATA (buffer);
  SCM_STRING_COERCE_0TERMINATION_X (str);
  cstr = SCM_STRING_CHARS(str); 
  if (cstr == NULL) {
    return SCM_BOOL_T;
  }
  pos = scm_num2ulong (position, SCM_ARG3, "minara-buffer-insert");
  // Make the undo action
  result = UndoBufferInsert (buffer, str, position);
  // Perform the insert
  t_insspos (b->text, pos, cstr);
  b->changed = 1;
  return result;
}

/**
   Find in a buffer
   FIXME!: This returns data rather than an undo!!!
*/

SCM minara_buffer_find (SCM buffer, SCM str, SCM position) {
  MinaraBuffer * b = NULL;
  char * cstr = "";
  unsigned int pos = 0;
  SCM result = SCM_EOL;
  unsigned int findPos = -1;
  SCM_ASSERT (SCM_SMOB_PREDICATE (minara_buffer_tag, buffer),
              buffer, SCM_ARG1, "minara-buffer-find");
  SCM_ASSERT(SCM_STRINGP(str), str, SCM_ARG2, "minara-buffer-find");
  SCM_ASSERT(SCM_NUMBERP(position), position, SCM_ARG3, "minara-buffer-find");
  b = (MinaraBuffer*) SCM_SMOB_DATA (buffer);
  SCM_STRING_COERCE_0TERMINATION_X (str);
  cstr = SCM_STRING_CHARS(str); 
  if (cstr == NULL) {
    return SCM_BOOL_T;
  }
  // Get the find-after position
  pos = scm_num2ulong (position, SCM_ARG3, "minara-buffer-find");
  // Find
  findPos = t_finds ( b->text, pos, 0, cstr);
  // Make the result
  result = scm_uint2num (findPos);
  return result;
}

/**
   Replace a range in a buffer
*/

SCM minara_buffer_replace (SCM buffer, SCM str, SCM replace) {
  MinaraBuffer * b = NULL;
  char * cstr = "";
  char * creplace = "";
  unsigned int pos = 0;
  SCM result = SCM_EOL;
  unsigned int findPos = -1;
  SCM_ASSERT (SCM_SMOB_PREDICATE (minara_buffer_tag, buffer),
              buffer, SCM_ARG1, "minara-buffer-replace");
  SCM_ASSERT(SCM_STRINGP(str), str, SCM_ARG2, "minara-buffer-replace");
  SCM_ASSERT(SCM_STRINGP(replace), replace, SCM_ARG3, "minara-buffer-replace");
  b = (MinaraBuffer*) SCM_SMOB_DATA (buffer);
  SCM_STRING_COERCE_0TERMINATION_X (str);
  cstr = SCM_STRING_CHARS(str);
  if (cstr == NULL) {
    return SCM_BOOL_T;
  }
  SCM_STRING_COERCE_0TERMINATION_X (replace);
  creplace = SCM_STRING_CHARS(replace);
  if (creplace == NULL) {
    return SCM_BOOL_T;
  }
  // Get the first match position
  pos = t_finds( b->text, 0, 0, cstr );
  if ( t_error )
    return SCM_BOOL_T;
  // Make the undo action
  result = UndoBufferReplace (buffer, str, replace);
  // Perform the replace
  t_repposns (b->text, pos, SCM_STRING_LENGTH(str), 
	      creplace);
  b->changed = 1;
  return result;
}

/**
   Replace a range of a buffer with a (different length of) string
*/

SCM minara_buffer_replace_range (SCM buffer, SCM replace, 
				 SCM position, SCM count) {
  MinaraBuffer * b = NULL;
  char * cstr = "";
  char * creplace = "";
  unsigned int p = 0;
  unsigned int c = 0;
  SCM result = SCM_EOL;
  SCM_ASSERT (SCM_SMOB_PREDICATE (minara_buffer_tag, buffer),
              buffer, SCM_ARG1, "minara-buffer-replace-range");
  SCM_ASSERT(SCM_STRINGP(replace), replace, SCM_ARG2, 
	     "minara-buffer-replace-range");
  SCM_ASSERT(SCM_NUMBERP(position), position, SCM_ARG3, 
	     "minara-buffer-replace-range");
  SCM_ASSERT(SCM_NUMBERP(count), count, SCM_ARG4, 
	     "minara-buffer-replace-range");
  b = (MinaraBuffer*) SCM_SMOB_DATA (buffer);
  SCM_STRING_COERCE_0TERMINATION_X (replace);
  creplace = SCM_STRING_CHARS(creplace);
  if (creplace == NULL) {
    return SCM_BOOL_T;
  }
  // Get the first match position
  p = scm_num2long (position, SCM_ARG3, "minara-buffer-replace-range");
  c = scm_num2long (count, SCM_ARG4, "minara-buffer-replace-range");
  if ( t_error )
    return SCM_BOOL_T;
  // Make the undo action
  result = UndoBufferReplaceRange (buffer, b->text, position, count);
  // Perform the replace
  t_repposns (b->text, p, c, 
	      SCM_STRING_CHARS(replace));
  b->changed = 1;
  return result;
}

/** 
    Find and replace ALL in a buffer
    TODO: De-c-string where possible
*/

SCM minara_buffer_replace_all (SCM buffer, SCM str, SCM replace) {
  MinaraBuffer * b = NULL;
  char * cstr = "";
  char * creplace = "";
  SCM result = SCM_EOL;
  unsigned int findPos = -1;
  SCM_ASSERT (SCM_SMOB_PREDICATE (minara_buffer_tag, buffer),
              buffer, SCM_ARG1, "minara-buffer-replace-all");
  SCM_ASSERT(SCM_STRINGP(str), str, SCM_ARG2, "minara-buffer-replace-all");
  SCM_ASSERT(SCM_STRINGP(replace), replace, SCM_ARG3, 
	     "minara-buffer-replace-all");
  b = (MinaraBuffer*) SCM_SMOB_DATA (buffer);
  SCM_STRING_COERCE_0TERMINATION_X (str);
  cstr = SCM_STRING_CHARS(str); 
  if (cstr == NULL) {
    return SCM_BOOL_T;
  }
  SCM_STRING_COERCE_0TERMINATION_X (replace);
  creplace = SCM_STRING_CHARS(replace); 
  if (creplace == NULL) {
    return SCM_BOOL_T;
  }
  // Make the undo action
  result = UndoBufferReplaceAll (buffer, str, replace);
  // Perform the replace all
  t_reps (b->text, 0, cstr, creplace);
  return result;
}

// Program lifecycle

/**
   Register our Guile functions
*/

void BufferStartup () {
  // Register our types
  minara_buffer_tag = 
    scm_make_smob_type ("minara-buffer", sizeof (MinaraBuffer));
  scm_set_smob_mark (minara_buffer_tag, 0);
  scm_set_smob_free (minara_buffer_tag, minara_buffer_destroy);
  // Register our scheme functions
  scm_c_define_gsubr ("buffer-make", 0, 0, 0, minara_buffer_make);
  scm_c_define_gsubr ("buffer-load", 2, 0, 0, minara_buffer_load);
  scm_c_define_gsubr ("buffer-save", 2, 0, 0, minara_buffer_save);
  scm_c_define_gsubr ("buffer-append", 2, 0, 0, minara_buffer_append);
  scm_c_define_gsubr ("buffer-prepend", 2, 0, 0, minara_buffer_prepend);
  scm_c_define_gsubr ("buffer-delete", 3, 0, 0, minara_buffer_delete);
  scm_c_define_gsubr ("buffer-insert", 3, 0, 0, minara_buffer_insert);
  scm_c_define_gsubr ("buffer-find", 3, 0, 0, minara_buffer_find);
  scm_c_define_gsubr ("buffer-replace", 3, 0, 0, minara_buffer_replace);
  scm_c_define_gsubr ("buffer-replace-range", 4, 0, 0, 
		      minara_buffer_replace_range);
  scm_c_define_gsubr ("buffer-replace-all", 3, 0, 0, minara_buffer_replace_all);
  // Get the Scheme functions we need to make undo actions into globals
  gBufferInsert = scm_c_eval_string ("buffer-insert");
  gBufferDelete = scm_c_eval_string ("buffer-delete");
  gBufferReplace = scm_c_eval_string ("buffer-replace");
  gBufferReplaceRange = scm_c_eval_string ("buffer-replace-range");
  gBufferReplaceAll = scm_c_eval_string ("buffer-replace-all");
}
