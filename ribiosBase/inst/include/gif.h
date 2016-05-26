/*****************************************************************************
* (c) Copyright 2012-2013 F.Hoffmann-La Roche AG                             *
* Contact: bioinfoc@bioinfoc.ch, Detlef.Wolf@Roche.com.                      *
*                                                                            *
* This file is part of BIOINFO-C. BIOINFO-C is free software: you can        *
* redistribute it and/or modify it under the terms of the GNU Lesser         *
* General Public License as published by the Free Software Foundation,       *
* either version 3 of the License, or (at your option) any later version.    *
*                                                                            *
* BIOINFO-C is distributed in the hope that it will be useful, but           *
* WITHOUT ANY WARRANTY; without even the implied warranty of                 *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
* Lesser General Public License for more details. You should have            *
* received a copy of the GNU Lesser General Public License along with        *
* BIOINFO-C. If not, see <http://www.gnu.org/licenses/>.                     *
*****************************************************************************/
/** @file gif.h
    @brief Creates GIF graphics.
    With the help of
    Die grosse Welt der Grafikformate
    Grafikprogrammierung unter Windows und WindowNT
    Thomas W. Lipp
    Synergy book ISBN 3-9803718-0-8.
    Module prefix gif_
*/
#ifndef GIF_H
#define GIF_H

#ifdef __cplusplus
extern "C" {
#endif

#include "bitmap.h"

extern BitmapObject gif_read (char *fn);
extern void gif_write (BitmapObject bmo,char *fn);
extern void gif_interlaceSet (int i);
extern void gif_globalColormapSet (int g);
extern void gif_transparentColorSet (int c);

#ifdef __cplusplus
}
#endif

#endif
