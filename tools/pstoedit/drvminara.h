#ifndef __drvminara_h
#define __drvminara_h

/* 
   drvminara.h : Minara driver for pstoedit

   Copyright (c) 2004 Rob Myers, rob@robmyers.org

   Based on:

   drvsampl.h : This file is part of pstoedit
   Class declaration for a sample output driver with no additional attributes
   and methods (minimal interface)

   Copyright (C) 1993 - 2003 Wolfgang Glunz, wglunz@pstoedit.net

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

*/

#include "drvbase.h"

class drvminara : public drvbase {

public:

	derivedConstructor(drvminara);
	//(const char * driveroptions_P,ostream & theoutStream,ostream & theerrStream ); // Constructor

	~drvminara(); // Destructor

#include "drvfuncs.h"
	void show_rectangle(const float llx, const float lly, const float urx, const float ury);
	void show_text(const TextInfo & textInfo);

public:

	virtual void    show_image(const PSImage & imageinfo); 

private:
	void print_coords();
	int	      imgcount;

};

#endif
 
 
 
