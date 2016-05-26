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
/** @file chtoken.h
    @brief Dissect a C header file into tokens.
    Module prefix cht_
*/
#ifndef CHTOKEN_H
#define CHTOKEN_H

#ifdef __cplusplus
extern "C" {
#endif

#include "format.h"

/// static
#define CHT_STATIC 1
/// char
#define CHT_CHAR 2
/// # define
#define CHT_DEFINE 3
/// NULL
#define CHT_NULL 4
/// *
#define CHT_STAR 5
/// \n
#define CHT_NL 6
/// ;
#define CHT_SEMICOLON 7
/// =
#define CHT_EQUAL 8
/// {
#define CHT_CURLYOPEN 9
/// }
#define CHT_CURLYCLOSE 10
/// [
#define CHT_BRACKETOPEN 11
/// ]
#define CHT_BRACKETCLOSE 12
/// (
#define CHT_PARENSOPEN 13
/// )
#define CHT_PARENSCLOSE 14
/// ,
#define CHT_COMMA 15
/// int
#define CHT_INT 16
/// /
#define CHT_SLASH 17
/// # ifdef
#define CHT_IFDEF 18
/// # ifndef
#define CHT_IFNDEF 19
/// # endif
#define CHT_ENDIF 20

/// a string constant
#define CHT_STRINGCONST 100
/// a character constant
#define CHT_CHARCONST 101
/// an integer constant
#define CHT_INTCONST 102
/// a comment /* */
#define CHT_COMMENT1 103
/// a comment //
#define CHT_COMMENT2 104
/// an identifier
#define CHT_IDENTIFIER 105

extern char cht_deEscapeChar (char *s,int *len);
extern char *cht_deEscapeString (char *s);
extern char *cht_fileReadContCat (char *fileName);

extern void cht_init (char *fileName);
extern char *cht_context (void);
extern int cht_getAll (Stringa value);
extern int cht_get (Stringa value);

#ifdef __cplusplus
}
#endif

#endif
