/*

LibText

Copyright (c) 2002 Ryan Daniels <omegaprojects@users.sourceforge.net>

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.



Libtext is a C library for creating, reading, and performing operations
on growable text / string buffer objects with optional error callbacks.
The library performs actions such as append, delete, find, get, insert,
prepend, replace, and reverse. Libtext can also append, insert, overwrite,
and prepend printf style formats. Further options include rejecting binary
characters, setting a maximum size, and setting a mask that controls for
which errors the callback will be executed. All of which can be set for
each individual object. Libtext is basic enough to be general purpose so
that it can be used to build more complex routines, while still being
simple enough to use as is in rudimentary applications with little effort,
learning the API within minutes.

*/


#ifndef __TEXT_H__
#define __TEXT_H__


#ifdef __cplusplus
extern "C" {
#endif


/* DEFINED LIBRARY ERRORS */

enum
{
	T_ERR_BIN = 1, /* ATTEMPT TO ADD BINARY CHAR */
	T_ERR_MAX = 2, /* ATTEMPT TO EXCEED MAXIMUM SIZE */
	T_ERR_MEM = 4, /* MEMORY ALLOCATION ERROR */
	T_ERR_NTF = 8  /* QUERIED TEXT NOT FOUND */
};


/* THESE TWO DEFINITIONS ARE INTENDED FOR INTERNAL LIBRARY USE ONLY */

enum
{
	__MODE_INS__,
	__MODE_OVR__
};


/* NOT USED AT THIS TIME */

/*extern char *t_errorlist[];*/


/* DEFINITION OF TEXT OBJECT */

struct _Text
{
	char *text; /* POINTER TO MEMORY USED BY OBJECT */

	unsigned int len; /* LENGTH OF TEXT - 0 FOR NO LIMIT */

	unsigned int size; /* SIZE OF MEMORY POINT TO BY TEXT */

	unsigned int indx; /* INTERNAL RW POSITION OF RW OPERATIONS ON TEXT */

	unsigned int max; /* MAXIMUM SIZE OF TEXT */

	unsigned int inc; /* SIZE IN WHICH TEXT MEMORY IS INCREMENTED - A VALUE OF 0 WILL
	                  ** INCREMENT IN BLOCKS OF 1024 */

	int rbin; /* REJECT OR NOT REJECT BINARY CHARS - BOOLEAN VALUE */

	int errmask; /* MASK THAT DETERMINES WHICH ERRORS WILL CAUSE errcall TO BE EXECUTED */

	void ( *errcall )( struct _Text *, int ); /* FUNCTION OPTIONALLY CALLED ON ERRORS */
};

typedef struct _Text Text;


/* GLOBAL ERROR CALLBACK - USED WHEN TEXT OBJECT'S INTERNAL errcall IS NULL */

extern void ( *t_errcall )( Text *, int );


/* SET BY LIBRARY WHEN ERROR OCCURS TO PREDEFINED ERRORS -
** SEE ERROR DEFINITIONS AT TOP, WILL BE SET TO ZERO WHEN
** NO ERROR OCCURS
*/

extern int t_error;


/* THESE TWO ITEMS ARE USED FOR DEBUGGING BY ME AND WILL EVENTUALLY BE REMOVED */

extern int SHOWBUFINFO;

extern void PrintTinfo( Text *text );


/********************************************
** This is only used for the t_putfmtn macro.
** Not intended for other than internal use
** by library. This is to avoid having  a
** separate version of __t_writefmt__ that
** accepts a va_list as an argument.
********************************************/

extern int __T_WRITEFMT_RET_VAL__;



/* *** MAIN API DOCUMENTATION ***
**
**
**  There are 7 main types of operations that modify the text in an
**  object, and each has an abbreviation used in the function names
**  to perform that operation:
**
**  app - to append text
**  del - to delete text
**  ins - to insert text
**  pre - to prepend text
**  put - to put / overwrite text
**  rep - to replace text
**  tru - to truncate text
**
**  There are 6  suffixes that will indicate how this operation will
**  be performed, which will also indicate the kind of arguments
**  the function will need:
**
**  c - operation performed with one char
**  s - operation performed with a string
**  fmt - operation performed with printf style format
**
**  n - operation performed up to N number of chars
**  pos - operation performed at POS in text
**
**  d - a single char delimiter only used by t_get calls
**
**  Each text object has it's own internal rw position. The functions
**  that use this value in dealing with the text are: t_get, t_ins, and
**  t_put, but only the versions of these that have no pos suffix. t_put
**  and t_get calls will always modify this value, whereas t_ins will never
**  modify it. Two other functions that can unknowingly modify the internal
**  rw position are t_del and t_tru. They can modify it if the text is
**  shortened to a length that is below the current value.
**
**  There are several other functions that perform many other useful tasks.
**  See the comments by each of these calls for further documentation.
**
**
** *************************/


/* T_APPC - APPEND CHAR CALLS */

#define t_appc( TEXT, C ) t_appcn( TEXT, 1, C )

#define t_appcn( TEXT, N, C ) __t_writec__( TEXT, TEXT->len, N, __MODE_OVR__, C )


/* T_APPS - APPEND STRING CALLS */

#define t_apps( TEXT, S ) t_appsn( TEXT, 0, S )

#define t_appsn( TEXT, N, S ) __t_writes__( TEXT, TEXT->len, N, __MODE_OVR__, S )


/* T_APPFMT - APPEND PRINTF STYLE FORMATING */

#define t_appfmt( TEXT, FMT, args... ) t_appfmtn( TEXT, 0, FMT, ##args )

#define t_appfmtn( TEXT, N, FMT, args... ) __t_writefmt__( TEXT, TEXT->len, N, __MODE_OVR__, FMT, ##args )


/* --------------------------------------------------------------- */


/* T_DEL - DELETE CHARS */

#define t_delc( TEXT ) t_delcn( TEXT, 1 )

#define t_delcn( TEXT, N ) t_delcposn( TEXT, TEXT->indx, N )

#define t_delcpos( TEXT, POS ) t_delcposn( TEXT, POS, 1 )

extern void t_delcposn( Text *text, unsigned int pos, unsigned int n );


/* --------------------------------------------------------------- */


/* T_FIND - FIND A CHAR OR STRING AND RETURN IT'S POSITION IN THE TEXT.
** t_error WILL BE SET TO T_ERR_NTF IF NOT FOUND AND ZERO WILL BE RETURNED,
** BUT NOTE THAT ZERO IS ALSO A VALID POSTION AT WHICH TO FIND TEXT. IN
** THAT CIRCUMSTANCE, t_error WILL BE ZERO AS WELL, INDICATING NO ERROR.
** icase TAKES A BOOLEAN VALUE TO INDICATE WHETHER OR NOT THE CASE WILL BE
** IGNORED, TRUE FOR YES, FALSE FOR NO.
*/

extern unsigned int t_findc( Text *text, unsigned int pos, int icase, char c );

extern unsigned int t_finds( Text *text, unsigned int pos, int icase, const char *s );


/* --------------------------------------------------------------- */


/* T_FREE - FREE TEXT OBJECT POINTED TO BY text */

extern void t_free( Text *text );


/* --------------------------------------------------------------- */


/* T_GETC - GET CHAR AT INTERNAL RW POSITION */

extern char t_getc( Text *text );

/* T_GETC - GET CHAR AT GIVEN RW POSITION */

extern char t_getcpos( Text *text, unsigned int pos );


/* T_GETS - GET STRING INTO SPECIFIED BUFFER */

#define t_getsn( TEXT, S, N ) t_getsnd( TEXT, N, '\n', S )

extern int t_getsnd( Text *text, unsigned int n, char d, char *s );

#define t_getsposn( TEXT, POS, N, S ) t_getsposnd( TEXT, POS, N, '\n', S )

extern int t_getsposnd( Text *text, unsigned int pos, unsigned int n, char d, char *s );


/* --------------------------------------------------------------- */


/* T_GETALL - RETURN POINTER TO ENTIRE TEXT */

#define t_getall( TEXT ) ( TEXT->text )

/* T_GETREM - RETURN POINTER TO REMAINDER OF TEXT STARTING AT INTERNAL RW POSITION */

#define t_getrem( TEXT ) ( &TEXT->text[ TEXT->indx ] )

/* T_GETREMPOS - RETURN POINTER TO REMAINDER OF TEXT STARTING AT GIVEN RW POSITION */

#define t_getrempos( TEXT, POS ) ( POS > TEXT->len ? &TEXT->text[ TEXT->len ] : &TEXT->text[ POS ] )

/* T_GETPOS - GET CURRENT INTERNAL RW POSITION */

#define t_getpos( TEXT ) ( TEXT->indx )


/* --------------------------------------------------------------- */


/* T_LEN - RETURN LENGTH OF TEXT */

#define t_len( TEXT ) ( TEXT->len )


/* --------------------------------------------------------------- */


/* T_NEW - CREATE NEW TEXT OBJECT WITH PREDEFINED VALUES
** PREDEFINED VALUES:
** MAX SIZE - 0 ( NO LIMIT )
** INCREMENT VALUE - 1024
** REJECT BINARY CHARS - TRUE
** ERRCALL WILL BE EXECUTED FOR T_ERR_BIN AND T_ERR_MEM
** ERRCALL SET TO NULL
*/

#define t_new() t_newdef( 0, 0, 1, T_ERR_BIN | T_ERR_MEM, NULL )


/* T_NEWDEF - DEFINE ALL THE OPTIONS FOR THE TEXT OBJECT MANUALLY
** SEE DEFINITION OF TEXT OBJECT AT TOP
*/

extern Text *t_newdef( unsigned int max, unsigned int inc, int rbin, int errmask, void ( *errcall )( Text *, int ) );


/* --------------------------------------------------------------- */


/* T_INSC - INSERT CHAR CALLS */

#define t_insc( TEXT, C ) t_inscpos( TEXT, TEXT->indx, C )

#define t_inscn( TEXT, N, C ) t_inscposn( TEXT, TEXT->indx, N, C )

#define t_inscpos( TEXT, POS, C ) t_inscposn( TEXT, POS, 1, C )

#define t_inscposn( TEXT, POS, N, C ) __t_writec__( TEXT, POS, N, __MODE_INS__, C )


/* T_INSS - INSERT STRING CALLS */

#define t_inss( TEXT, S ) t_insspos( TEXT, TEXT->indx, S )

#define t_inssn( TEXT, N, S ) t_inssposn( TEXT, TEXT->indx, N, S )

#define t_insspos( TEXT, POS, S ) t_inssposn( TEXT, POS, 0, S )

#define t_inssposn( TEXT, POS, N, S ) __t_writes__( TEXT, POS, N, __MODE_INS__, S )


/* T_INSFMT - INSERT PRINTF STYLE FORMATING INTO TEXT */

#define t_insfmt( TEXT, FMT, args... ) t_insfmtn( TEXT, 0, FMT, ##args )

#define t_insfmtn( TEXT, N, FMT, args... ) t_insfmtposn( TEXT, TEXT->indx, N, FMT, ##args )

#define t_insfmtpos( TEXT, POS, FMT, args... ) t_insfmtposn( TEXT, POS, 0, FMT, ##args )

#define t_insfmtposn( TEXT, POS, N, FMT, args... ) __t_writefmt__( TEXT, POS, N, __MODE_INS__, FMT, ##args )


/* --------------------------------------------------------------- */


/* T_PREC - PREPEND C CALLS */

#define t_prec( TEXT, C ) t_precn( TEXT, 1, C )

#define t_precn( TEXT, N, C ) __t_writec__( TEXT, 0, N, __MODE_INS__, C )


/* T_PRES - PREPEND S CALLS */

#define t_pres( TEXT, S ) t_presn( TEXT, 0, S )

#define t_presn( TEXT, N, S ) __t_writes__( TEXT, 0, N, __MODE_INS__, S )


/* T_PREFMT - PREPEND PRINTF STYLE FORMATING */

#define t_prefmt( TEXT, FMT, args... ) t_prefmtn( TEXT, 0, FMT, ##args )

#define t_prefmtn( TEXT, N, FMT, args... ) __t_writefmt__( TEXT, 0, N, __MODE_INS__, FMT, ##args )


/* --------------------------------------------------------------- */


/* T_PUTC CALLS */

#define t_putc( TEXT, C ) t_putcn( TEXT, 1, C )

extern int t_putcn( Text *text, unsigned int n, char c );

#define t_putcpos( TEXT, POS, C ) t_putcposn( TEXT, POS, 1, C )

#define t_putcposn( TEXT, POS, N, C ) __t_writec__( TEXT, POS, N, __MODE_OVR__, C )


/* T_PUTS CALLS */

#define t_puts( TEXT, S ) t_putsn( TEXT, 0, S )

extern int t_putsn( Text *text, unsigned int n, const char *s );

#define t_putspos( TEXT, POS, S ) t_putsposn( TEXT, POS, 0, S )

#define t_putsposn( TEXT, POS, N, S ) __t_writes__( TEXT, POS, N, __MODE_OVR__, S )


/* T_PUTFMT - PUT PRINTF STYLE FORMATING INTO TEXT */

#define t_putfmt( TEXT, FMT, args... ) t_putfmtn( TEXT, 0, FMT, ##args )

#define t_putfmtn( TEXT, N, FMT, args... ) \
( __T_WRITEFMT_RET_VAL__ = __t_writefmt__( TEXT, TEXT->indx, N, __MODE_OVR__, FMT, ##args ), TEXT->indx += __T_WRITEFMT_RET_VAL__, __T_WRITEFMT_RET_VAL__ )

#define t_putfmtpos( TEXT, POS, FMT, args... ) t_putfmtposn( TEXT, POS, 0, FMT, ##args )

#define t_putfmtposn( TEXT, POS, N, FMT, args... ) __t_writefmt__( TEXT, POS, N, __MODE_OVR__, FMT, ##args )


/* --------------------------------------------------------------- */


/* T_REP - REPLACE CALLS */

extern int t_repc( Text *text, int icase, char rep, char with );

extern int t_repposns( Text *text, unsigned int pos, unsigned int n, const char *s );

extern int t_reps( Text *text, int icase, char *reps, char *withs );


/* --------------------------------------------------------------- */


/* REVERSE CONTAINED TEXT */

void t_reverse( Text *text );


/* --------------------------------------------------------------- */


/* T_SETERRCALL - SET ERROR CALLBACK FOR TEXT OBJECT */

#define t_seterrcall( TEXT, ERRFUN ) ( { TEXT->errcall = ERRFUN; } )


/* T_SETPOS - SET INTERNAL RW POSITION OF TEXT OBJECT */

extern void t_setpos( Text *text, unsigned int indx );


/* --------------------------------------------------------------- */


/* T_TO{UPPER,LOWER} - NO NEED TO EXPLAIN HERE */

extern void t_tolower( Text *text );

extern void t_toupper( Text *text );


/* --------------------------------------------------------------- */


/* T_TRU - TRUNCATE TEXT */

#define t_tru( TEXT ) t_trupos( TEXT, TEXT->indx )

#define t_trupos( TEXT, POS ) t_delcposn( TEXT, POS, 0 )


/* --------------------------------------------------------------- */


/* T_ZERO - ZEROS LEN AND INTERNAL RW POSITION, AND LEAVES MEMORY IN TACT FOR RE-USE */

extern void t_zero( Text *text );


/* --------------------------------------------------------------- */


/* MAIN CORE OF LIBRARY - NOT INTENDED FOR DIRECT USE */

extern int __t_writec__( Text *text, unsigned int pos, unsigned int n, int mode, char c );

extern int __t_writes__( Text *text, unsigned int pos, unsigned int n, int mode, const char *s );

extern int __t_writefmt__( Text *text, unsigned int pos, unsigned int n, int mode, const char *fmt, ... );

//extern int __t_writefmt2__( Text *text, unsigned int pos, unsigned int n, int mode, const char *fmt, ... );


#ifdef __cplusplus
}
#endif


#endif /* __TEXT_H__ */


