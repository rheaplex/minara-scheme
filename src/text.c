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

#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "text.h"

#define T_HANDLE_ERR( TEXT, ERR )\
({                               \
t_error = ERR;                   \
                                 \
if ( TEXT->errmask & ERR &&      \
     TEXT->errcall != NULL )     \
{                                \
	TEXT->errcall( TEXT, ERR );  \
}                                \
else if ( TEXT->errmask & ERR && \
          t_errcall != NULL )    \
{                                \
	t_errcall( TEXT, ERR );      \
}                                \
})

#define ISBIN_C( C ) ( !isgraph( C ) &&  !isspace( C ) )


/* T_ERRCALL - GLOBAL ERROR CALL */

void ( *t_errcall )( Text *, int ) = NULL;


/* T_ERROR - SET TO VALUES DEFINED IN TEXT.H ON RESPECTIVE ERRORS */

int t_error = 0;

/* FOR DEBUGGING, WILL BE REMOVED */

int SHOWBUFINFO = 0;

/********************************************
** This is only used for the t_putfmtn macro.
** Not intended for other than internal use
** by library. This is to avoid having  a
** separate version of __t_writefmt__ that
** accepts a va_list as an argument.
********************************************/

int __T_WRITEFMT_RET_VAL__ = 0;

/*
char *t_errorlist[] = {
	"",
	"attempt to add binary character",
	"maximum sized reached",
	"memory allocation error"
};
*/

#define CHECK_BIN( C, RET )         \
({                                  \
if ( ISBIN_C( C ) )                 \
{                                   \
	T_HANDLE_ERR( text, T_ERR_BIN );\
                                    \
	RET                             \
}                                   \
})

#define CHECK_MAX( RET )                  \
({                                        \
if ( text->max && text->len == text->max )\
{                                         \
	T_HANDLE_ERR( text, T_ERR_MAX );      \
                                          \
	RET                                   \
}                                         \
})


void PrintTinfo( Text *text )
{
	printf( "\nlen: %d\nsize: %d\ninc: %d\nmax: %d\nindx: %d\n\n",
	         text->len, text->size, text->inc, text->max, text->indx );

	printf( "TEXT: %s(END)\n\n============\n", t_getall( text ) );
}


/* *** TEXT.C PRIVATE FUNCTIONS *** */

static void BufInfo( char *buf, unsigned int size );

static void t_moveover( Text *text, unsigned int pos, unsigned int n );

static int t_resize( Text *text, unsigned int count );



void t_delcposn( Text *text, unsigned int pos, unsigned int n )
{
	if ( n && pos + n < text->len )
	{
		strcpy( &text->text[ pos ], &text->text[ pos + n ] );

		text->len -= n;

		if ( text->indx > text->len )
			text->indx = text->len;
	}
	else if ( pos < text->len ) /* n == 0 here */
	{
		unsigned int rem_len;

		rem_len = strlen( &text->text[ pos ] );

		text->text[ pos ] = '\0';

		text->len -= rem_len;

		if ( text->indx > text->len )
			text->indx = text->len;
	}
}

unsigned int t_findc( Text *text, unsigned int pos, int icase, char c )
{
	int x;
	unsigned int tlen = text->len;
	static unsigned int found = 0;

	//printf( "Calling FINDC: pos: %d, icase: %d, c: %c\n", pos, icase, c );

	if ( pos < tlen )
	{

		switch ( icase )
		{
			case 0:

				for ( x = pos; x < tlen; x++ )
				{
					//printf( "FINDC: Comparing: %c to: %c\n", text->text[ x ], c );

					if ( text->text[ x ] == c )
					{
						found = x;

						t_error = 0;

						return found;
					}
				}

				break;

			default:

				for ( x = pos; x < tlen; x++ )
				{
					if ( toupper( text->text[ x ] ) == toupper( c ) )
					{
						found = x;

						t_error = 0;

						return found;
					}
				}

				break;
		}
	}

	T_HANDLE_ERR( text, T_ERR_NTF );

	return found;
}

unsigned int t_finds( Text *text, unsigned int pos, int icase, const char *s )
{
	unsigned int found;
	int slen = strlen( s );

	if ( slen )
	{
		switch ( icase )
		{
			case 0:

				while ( 1 )
				{
					found = t_findc( text, pos, icase, *s );

					if ( t_error == T_ERR_NTF )
						break;

					if ( strncmp( &text->text[ found ], s, slen ) == 0 )
						return found;

					pos++;
				}

				break;

			default:

				while ( 1 )
				{
					found = t_findc( text, pos, icase, *s );

					if ( t_error == T_ERR_NTF )
						break;

					if ( strncasecmp( &text->text[ found ], s, slen ) == 0 )
						return found;

					pos++;
				}

				break;
		}
	}

	return found;
}

void t_free( Text *text )
{
	free( text->text );

	free( text );
}

char t_getc( Text *text )
{
	char ret;

	ret = text->text[ text->indx ];

	if ( ret )
		text->indx++;

	return ret;
}

char t_getcpos( Text *text, unsigned int pos )
{
	if ( pos > text->len )
		pos = text->len;

	return text->text[ pos ];
}

int t_getsnd( Text *text, unsigned int n, char d, char *s )
{
	int num;

	num = t_getsposnd( text, text->indx, n, d, s );

	text->indx += num;

	return num;
}

int t_getsposnd( Text *text, unsigned int pos, unsigned int n, char d, char *s )
{
	int num = 0;
	char *from;
	char c;

	if ( pos < text->len )
	{
		from = &text->text[ pos ];

		c = *from;

		while ( n && c )
		{
			*s = c;

			s++;

			num++;

			if ( c == d )
				break;

			from++;

			n--;

			c = *from;
		}

		*s = '\0';
	}

	return num;
}

Text *t_newdef( unsigned int max, unsigned int inc, int rbin, int errmask, void ( *errcall )( Text *, int ) )
{
	Text *text = calloc( 1, sizeof ( Text ) );

	if ( text != NULL )
	{
		/*
		** Members text->len and text->indx
		** are already 0 from calloc
		*/

		/*
		** Set max and errcall to given values.
		** If max is 0 and errcall are NULL, 0
		** will be retained from calloc
		*/

		if ( max )
		{
			if ( inc && inc < max )
				text->inc = inc;
			else
				text->inc = max;

			text->max = max;
		}
		else
		{
			if ( inc )
				text->inc = inc;
			else
				text->inc = 1024;
		}

		text->text = calloc( text->inc + 1, sizeof ( char ) );

		if ( text->text == NULL )
		{
			free( text );

			return NULL;
		}

		text->size = text->inc;

		text->rbin = rbin ? 1 : 0;

		text->errmask = errmask;

		text->errcall = errcall;
	}

	//t_error = 0; /* ??? Does this go here ??? */

	return text;
}

int t_putcn( Text *text, unsigned int n, char c )
{
	int num;

	num = __t_writec__( text, text->indx, n, __MODE_OVR__, c );

	text->indx += num;

	return num;
}

int t_putsn( Text *text, unsigned int n, const char *s )
{
	int num;

	num = t_putsposn( text, text->indx, n, s );

	text->indx += num;

	return num;
}

int t_repc( Text *text, int icase, char rep, char with )
{
	char *text_ptr;
	unsigned int rep_count = 0;
	unsigned int x;

	if ( !ISBIN_C( rep ) && !ISBIN_C( with ) )
	{
		x = text->len;

		text_ptr = &text->text[ x - 1 ];

		switch ( icase )
		{
			case 0:

				while ( x-- )
				{
					if ( *text_ptr == rep )
					{
						*text_ptr = with;

						rep_count++;
					}

					text_ptr--;
				}

				break;

			default:

				while ( x-- )
				{
					if ( toupper( *text_ptr ) == toupper( rep ) )
					{
						*text_ptr = with;

						rep_count++;
					}

					text_ptr--;
				}

				break;
		}
	}
	//else
		//printf( "REPC: binc exit\n" );

	return rep_count;
}

int t_repposns( Text *text, unsigned int pos, unsigned int n, const char *s )
{
	unsigned int diff;
	unsigned int newslen = strlen( s );
	int ret_val = 0;

	if ( n == 0 )
		return 0;

	if ( newslen == 0 )
		t_delcposn( text, pos, n );
	else if ( newslen > n )
	{
		diff = newslen - n;

		t_putsposn( text, pos, n, s );

		if ( t_error )
			return -1;

		pos += n;

		s += n;

		t_inssposn( text, pos, diff, s );

		if ( t_error )
			return -1;
	}
	else if ( n > newslen )
	{
		diff = n - newslen;

		t_delcposn( text, pos, diff );

		t_putspos( text, pos, s );

		if ( t_error )
			return -1;
	}
	else
	{
		t_putspos( text, pos, s );

		if ( t_error )
			return -1;
	}

	return 0;
}

int t_reps( Text *text, int icase, char *reps, char *withs )
{
	int num = 0;
	unsigned int pos = 0;
	unsigned int reps_len = strlen( reps );
	unsigned int withs_len = strlen( withs );

	while ( 1 )
	{
		pos = t_finds( text, pos, icase, reps );

		if ( t_error )
			break;

		t_repposns( text, pos, reps_len, withs );

		//printf( "Found at: %d\n", pos );

		if ( t_error )
			break;

		pos += withs_len;

		num++;
	}

	num;
}

#ifdef USE_OLD_REPS
int t_reps( Text *text, int icase, char *reps, char *withs )
{
	unsigned int diff;
	unsigned int pos = 0;
	unsigned int rep_count = 0;
	unsigned int reps_len = strlen( reps );
	//unsigned int reps_len_1;
	unsigned int withs_len = strlen( withs );
	//unsigned int withs_len_1;
	int type = 0;

	if ( reps_len && withs_len && *reps && *withs )
	{
		//reps_len_1 = reps_len - 1;

		//withs_len_1 = withs_len - 1;

		if ( reps_len > withs_len )
			type = -1;
		else if ( reps_len < withs_len )
			type = 1;

		//printf( "W: %d, W1: %d\n", withs_len, withs_len_1 );

		//printf( "TLEN: %d\n", text->len );

		//printf( "T: %s, POS: %d\n", text->text, pos );

		switch ( icase )
		{
			case 0:

				while ( pos < text->len/*tlen--*/ )
				{
					if ( text->text[ pos ] == *reps
					&& strncmp( reps, &text->text[ pos ], reps_len ) == 0 )
					{
						switch ( type )
						{
							case -1:

								//printf( "CASE -1:\n" );

								diff = reps_len - withs_len;

								t_delcposn( text, pos, diff );

								t_putspos( text, pos, withs );

								pos += withs_len;

								break;

							case 1:

								//printf( "CASE 1:\n" );

								diff = withs_len - reps_len;

								t_putsposn( text, pos, reps_len, withs );

								pos += reps_len;

								t_insspos( text, pos, &withs[ reps_len ] );

								pos += diff;

								break;

							default:

								//printf( "CASE 0:\n" );

								t_putspos( text, pos, withs );

								pos += withs_len;

								break;
						}

						rep_count++;
					}
					else
					{
						//printf( "ELSE: text->text[ %d ]: %c\n", pos, text->text[ pos ] );
						pos++;
					}

					//printf( "POS: %d, T->LEN: %d\n", pos, text->len );
				}

				break;

			default:

				while ( pos < text->len/*tlen--*/ )
				{
					if ( toupper( text->text[ pos ] ) == toupper( *reps )
					&& strncasecmp( reps, &text->text[ pos ], reps_len ) == 0 )
					{
						switch ( type )
						{
							case -1:

								//printf( "CASE -1:\n" );

								diff = reps_len - withs_len;

								t_delcposn( text, pos, diff );

								t_putspos( text, pos, withs );

								pos += withs_len;

								break;

							case 1:

								//printf( "CASE 1:\n" );

								diff = withs_len - reps_len;

								t_putsposn( text, pos, reps_len, withs );

								pos += reps_len;

								t_insspos( text, pos, &withs[ reps_len ] );

								pos += diff;

								break;

							default:

								//printf( "CASE 0:\n" );

								t_putspos( text, pos, withs );

								pos += withs_len;

								break;
						}
					}
					else
					{
						//printf( "ELSE: text->text[ %d ]: %c\n", pos, text->text[ pos ] );
						pos++;
					}

					//printf( "POS: %d, T->LEN: %d\n", pos, text->len );
				}

				break;
		}
	}

	return rep_count;
}
#endif

void t_reverse( Text *text )
{
	unsigned int begin = 0;
	unsigned int end;
	unsigned int middle;
	char tmpc;

	if ( text->len < 2 )
		return;

	end = text->len - 1;
	middle = text->len / 2;

	while ( begin < middle )
	{
		tmpc = text->text[ begin ];

		text->text[ begin ] = text->text[ end ];

		text->text[ end ] = tmpc;

		begin++;

		end--;
	}
}

void t_tolower( Text *text )
{
	unsigned int pos = text->len;

	//printf( "Len: %u\n", pos );

	while ( pos-- )
	{
		//printf( "Editing pos: %u\n", pos );
		text->text[ pos ] = tolower( text->text[ pos ] );
	}
}

void t_toupper( Text *text )
{
	unsigned int pos = text->len;

	//printf( "Len: %u\n", pos );

	while ( pos-- )
	{
		//printf( "Editing pos: %u\n", pos );
		text->text[ pos ] = toupper( text->text[ pos ] );
	}
}

int __t_writec__( Text *text, unsigned int pos, unsigned int n, int mode, char c )
{
	unsigned int check_pos;
	unsigned int end_pos;
	unsigned int inc_count = 0;
	unsigned int max_reached = 0;
	unsigned int max_set = text->max;
	unsigned int needed_size;
	unsigned int num;
	unsigned int size;

	if ( SHOWBUFINFO )
		printf( "\nUSING SIZE WRITEC\n*****************\n" );

	if ( text->rbin )
		CHECK_BIN( c, return 0; );

	CHECK_MAX( return 0; );

	switch ( n )
	{
		case 0:

			n++;
	}

	//printf( "MADE IT: 0\n" );

	/*
	pos > text->len must be inside switch cases so
	it won't get be evaluated if the wrong mode is
	passed ( being before switch ), and it has to
	evaluate pos for the case.
	*/

	if ( SHOWBUFINFO )
		BufInfo( text->text, text->size );

	switch ( mode )
	{
		case __MODE_INS__:

			if ( pos > text->len )
				pos = text->len;

			check_pos = text->len;

			break;

		case __MODE_OVR__:

			if ( pos > text->len )
				pos = text->len;

			check_pos = pos;

			break;

		default:

			return 0;
	}

	//printf( "MADE IT: 1\n" );

	if ( max_set )
	{
		if ( check_pos + n > max_set )
		{
			num = max_set - check_pos;

			max_reached = 1;
		}
		else
			num = n;
	}
	else
		num = n;

	//printf( "NUM: %d, check_pos: %d, max_set: %d\n", num, check_pos, max_set );

	//printf( "writec: enter: text->len: %u\n", text->len );

	end_pos = check_pos + num;

	switch ( mode )
	{
		case __MODE_INS__:

			needed_size = text->len + num;

			break;

		case __MODE_OVR__:

			if ( end_pos >= text->len )
				needed_size = end_pos;
			else
				needed_size = text->len;

			break;
	}

	size = text->size;

	while ( size < needed_size )
	{
		size += text->inc;

		inc_count++;
	}

	if ( t_resize( text, inc_count ) )
	{
		T_HANDLE_ERR( text, T_ERR_MEM );

		return 0;
	}

	if ( SHOWBUFINFO )
		BufInfo( text->text, text->size );

	switch ( mode )
	{
		case __MODE_INS__:

			t_moveover( text, pos, num );

			if ( end_pos > text->len )
				text->text[ end_pos ] = '\0';

			text->len += num;

			break;

		case __MODE_OVR__:

			if ( end_pos > text->len )
			{
				text->text[ end_pos ] = '\0';

				text->len = end_pos;
			}

			break;
	}

	if ( SHOWBUFINFO )
		BufInfo( text->text, text->size );

	n = num; /* save how many chars written - reusing n */

	while ( num-- )
		text->text[ pos++ ] = c;

	if ( SHOWBUFINFO )
		BufInfo( text->text, text->size );

	if ( max_reached )
		T_HANDLE_ERR( text, T_ERR_MAX );
	else
		t_error = 0;

	//printf( "writec: exit: text->len: %u\n", text->len );

	return n; /* return number of chars written */
}

int __t_writes__( Text *text, unsigned int pos, unsigned int n, int mode, const char *s )
{
	char *s_ptr = ( char * )s;
	unsigned int bin_c_found = 0;
	unsigned int check_pos;
	unsigned int end_pos;
	unsigned int inc_count = 0;
	unsigned int max_reached = 0;
	unsigned int max_set = text->max;
	unsigned int needed_size;
	unsigned int num = 0;
	unsigned int size;
	int rbin = text->rbin;


	if ( SHOWBUFINFO )
		printf( "\nUSING SIZE WRITES\n*****************\n" );

	CHECK_MAX( return 0; );

	if ( SHOWBUFINFO )
		BufInfo( text->text, text->size );

	switch ( mode )
	{
		case __MODE_INS__:

			if ( pos > text->len )
				pos = text->len;

			check_pos = text->len;

			break;

		case __MODE_OVR__:

			if ( pos > text->len )
				pos = text->len;

			check_pos = pos;

			break;

		default:

			return 0;
	}

	//printf( "AV: %d\n", av );

	//BufInfo( text->text, text->size );

	while ( *s_ptr )
	{
		if ( rbin && ISBIN_C( *s_ptr ) )
		{
			bin_c_found = 1;

			break;
		}

		if ( n )
			if ( num == n )
				break;

		num++;

		s_ptr++;
	}

	//printf( "NUM: %d\n", num );

	if ( num )
	{
		end_pos = check_pos + num;

		if ( max_set )
			if ( end_pos /*check_pos + num*/ > max_set )
			{
				num = max_set - check_pos;

				end_pos = check_pos + num;

				max_reached = 1;
			}

		switch ( mode )
		{
			case __MODE_INS__:

				needed_size = text->len + num;

				break;

			case __MODE_OVR__:

				if ( end_pos > text->len )
					needed_size = end_pos;
				else
					needed_size = text->len;

				break;
		}

		size = text->size;

		while ( size < needed_size )
		{
			size += text->inc;

			inc_count++;
		}

		//printf( "\n\nNeeded_size: %d, num: %d\n\n", needed_size, num );

		if ( inc_count )
		{
			if ( t_resize( text, inc_count ) )
			{
				T_HANDLE_ERR( text, T_ERR_MEM );

				return 0;
			}
		}

		//BufInfo( text->text, text->size );

	if ( SHOWBUFINFO )
		BufInfo( text->text, text->size );

		switch ( mode )
		{
			case __MODE_INS__:

				if ( text->len )
					t_moveover( text, pos, num );

				/*
				** We only want to add a null char if
				** we are inserting after the last char
				** in the string ( pos == text->len );
				*/

				if ( /*end_pos*/ pos == text->len )
				{
					text->text[ end_pos ] = '\0';

					//printf( "Adding null char for insert\n" );
				}

				text->len += num;

				break;

			case __MODE_OVR__:

				if ( max_set && end_pos > max_set )
				{
					text->text[ max_set ] = '\0';

					text->len = max_set;
				}
				else if ( end_pos > text->len )
				{
					text->text[ end_pos ] = '\0';

					text->len = end_pos;

					//printf( "end_pos: %d\n", end_pos );
				}

				break;
		}

	if ( SHOWBUFINFO )
		BufInfo( text->text, text->size );

		//BufInfo( text->text, text->size );

		//printf( "STRNCPY: POS: %d, S: %s, NUM: %d\n", pos, s, num );
		strncpy( &text->text[ pos ], s, num );
		//printf( "NEW_TEXT: %s\n", text->text );

		//BufInfo( text->text, text->size );
	if ( SHOWBUFINFO )
		BufInfo( text->text, text->size );
	}

	if ( max_reached )
		T_HANDLE_ERR( text, T_ERR_MAX );
	else if ( bin_c_found )
		T_HANDLE_ERR( text, T_ERR_BIN );
	else
		t_error = 0;

	return num;
}

//#define OLD_WRITEFMT 1

#ifndef OLD_WRITEFMT
int __t_writefmt__( Text *text, unsigned int pos, unsigned int n, int mode, const char *fmt, ... )
{
	va_list vl;
	unsigned int num = 0;
	char *tmp;
	//char buf[204800];

	CHECK_MAX( return 0; );

	va_start( vl, fmt );

	num = vsnprintf( NULL, 0,  fmt, vl );

	va_end( vl );

	//printf( "T_WRITEFMT: first vsnprintf return: %d\n", num );

	if ( num )
	{
		tmp = malloc( num + 1 );

		if ( tmp == NULL )
		{
			T_HANDLE_ERR( text, T_ERR_MEM );

			return 0;
		}

		va_start( vl, fmt );

		vsnprintf( tmp, num + 1,  fmt, vl );

		va_end( vl );

		num = __t_writes__( text, pos, n, mode, tmp );

		free( tmp );
	}

	return num;
}
#endif

#ifdef OLD_WRITEFMT
int __t_writefmt__( Text *text, unsigned int pos, unsigned int n, int mode, const char *fmt, ... )
{
	va_list vl;
	char dc = 0;
	unsigned int bin_c_found = 0;
	unsigned int check_pos;
	unsigned int inc_count = 0;
	unsigned int max_reached = 0;
	unsigned int max_set = text->max;
	unsigned int needed_size;
	unsigned int num = 0;
	unsigned int end_pos;
	unsigned int size;

	//printf( "\nUSING SIZE WRITEFMT\n*****************\n" );

	CHECK_MAX( return 0; );

	if ( SHOWBUFINFO )
		BufInfo( text->text, text->size );

	//BufInfo( text->text, text->size );

	switch ( mode )
	{
		case __MODE_INS__:

			if ( pos > text->len )
				pos = text->len;

			check_pos = text->len;

			break;

		case __MODE_OVR__:

			if ( pos > text->len )
				pos = text->len;

			check_pos = pos;

			break;

		default:

			return 0;
	}

	va_start( vl, fmt );

	num = vsnprintf( &text->text[ pos ], 0,  fmt, vl );

	//printf( "T_WRITEPRINTF: vsnprintf returned: %d\n", num );

	va_end( vl );

	if ( n && num > n )
		num = n;

	if ( num )
	{
		end_pos = check_pos + num;

		if ( max_set )
			if ( end_pos > max_set )
			{
				num = max_set - check_pos;

				end_pos = check_pos + num;

				max_reached = 1;
			}

		/* Calculate needed size of text->text */

		/* The trailing '\0' of vsnprintf is a major
		** inconvenience here. */

		switch ( mode )
		{
			case __MODE_INS__:

				if ( pos >= text->len )
					needed_size = text->len + num;
				else
					needed_size = text->len + num + 1;

				break;

			case __MODE_OVR__:

				if ( end_pos >= text->len )
					needed_size = end_pos;
				else
				{
					needed_size = text->len;

					/************************************
					** Save char that will be overwritten
					** by trailing '\0' of vsnprintf.
					************************************/

					dc = text->text[ end_pos ];
				}

				break;
		}

		size = text->size;

		//printf( "SIZE before loop: %d, needed_size: %d\n", size, needed_size );

		while ( size < needed_size )
		{
			size += text->inc;

			inc_count++;
		}

		//printf( "SIZE after loop: %d\n", size );

		if ( inc_count )
		{
			if ( t_resize( text, inc_count ) )
			{
				T_HANDLE_ERR( text, T_ERR_MEM );

				return 0;
			}

			//BufInfo( text->text, text->size );
		}

		if ( SHOWBUFINFO )
			BufInfo( text->text, text->size );

		switch ( mode )
		{
			case __MODE_INS__:

				if ( text->len )
				{
					//printf( "MOVING OVER: %d\n", num + 1 );
					t_moveover( text, pos, num + 1 );

					//BufInfo( text->text, text->size );
				}

				break;

			//case __MODE_OVR__:



				//break;
		}

	if ( SHOWBUFINFO )
		BufInfo( text->text, text->size );

		//BufInfo( text->text, text->size );

		va_start( vl, fmt );

		//printf( "T_WRITEPRINTF: vsnprintf: pos: %d, num: %d ( + 1: %d )\n", pos, num, num + 1 );

		vsnprintf( &text->text[ pos ], num + 1, fmt, vl );

		va_end( vl );

		//BufInfo( text->text, text->size );

		//printf( "COPYING TO: %d, FROM: %d\n", pos + num, pos + num + 1 );

	if ( SHOWBUFINFO )
		BufInfo( text->text, text->size );

		/**********************
		** Post write clean-up.
		**********************/

		switch ( mode )
		{

			case __MODE_INS__:

				//printf( "pos: %d, num: %d\n", pos, num );

				//printf( "pos + num: %d, ( + 1 ): %d\n", pos + num, pos + num + 1 );

				if ( pos < text->len )
					strcpy( &text->text[ pos + num ], &text->text[ pos + num + 1 ] );

				text->len += num;

				break;

			case __MODE_OVR__:

				if ( dc )
				{
					//printf( "PUTTING: '%c' at: %d\n", dc, end_pos );

					text->text[ end_pos ] = dc;
				}

				if ( max_set && end_pos > max_set )
					text->len = max_set;
				else if ( end_pos > text->len )
					text->len = end_pos;

				break;
		}

	if ( SHOWBUFINFO )
		BufInfo( text->text, text->size );

		//BufInfo( text->text, text->size );
	}

	if ( max_reached )
		T_HANDLE_ERR( text, T_ERR_MAX );
	else if ( bin_c_found )
		T_HANDLE_ERR( text, T_ERR_BIN );
	else
		t_error = 0;

	return num;
}
#endif

void t_setpos( Text *text, unsigned int pos )
{
	if ( pos > text->len )
		text->indx = text->len;
	else
		text->indx = pos;
}

void t_zero( Text *text )
{
	text->text[ 0 ] = '\0';

	text->len = 0;

	text->indx = 0;
}



/* *** TEXT.C PRIVATE FUNCTIONS *** */

static void BufInfo( char *buf, unsigned int size )
{
	unsigned int x;

	//printf( "BUFFER: size: %d\n", size );

	x = 0;

	while ( 1 )
	{
		//printf( "[ %d ]: ", x );

		if ( buf[ x ] == '\0' )
			printf( "'\\0'" );
		else if ( ISBIN_C( buf[ x ] ) )
			printf( "%.2X", buf[ x ] );
		else
			printf( "'%c'", buf[ x ] );

		x++;

		if ( x == size + 1 )
			break;

		printf( ",%*c", buf[ x ] == 0 ? 2 : 3, ' ' );
	}

	putchar( '\n' );
}

static void t_moveover( Text *text, unsigned int pos, unsigned int n )
{
	char *dest;
	char *src;
	unsigned int x;
	int swaps = 0;

	//printf( "Calling t_moveover: text->len: %d, pos: %d, n: %d\n", text->len, pos, n );

	//printf( "text->size: %d\n", text->size );

	dest = &text->text[ text->len - 1 + n ];

	src = &text->text[ text->len - 1 ];

	x = text->len;

	while ( x > pos )
	{
		//printf( "X: %d\n", x );

		*dest-- = *src--;

		switch ( x )
		{
			case 0:

				return;
		}

		x--;

		swaps++;
	}

	//printf( "Moveover made: %d swaps\n" );
}

static int t_resize( Text *text, unsigned int count )
{
	char *new_text;
	unsigned int added_size;

	added_size = text->inc * count;

	new_text = calloc( text->size + added_size + 1, 1 );

	if ( new_text != NULL )
	{
		strcpy( new_text, text->text );

		free( text->text );

		text->text = new_text;

		text->size += added_size;

		return 0;
	}

	return -1;
}

#undef T_HANDLE_ERR

#undef ISBIN_C

#undef CHECK_BIN

#undef CHECK_MAX
