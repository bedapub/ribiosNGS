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
/** @file regularexpression.h
    @brief Module to handle regular expressions.
    Examples for regular expressions:
    "^seq1 = ([^,]+), ([0-9]+) bp$".
    Module prefix regex_
*/
#ifndef REGULAREXPRESSION_H
#define REGULAREXPRESSION_H

#ifdef __cplusplus
extern "C" {
#endif

#include "format.h"

int regex_match_func (char *s1,char *s2,char *flags,Texta out,
                      Array begin,Array end);
/// convenience macro
#define regex_match(s1,s2,flags,out) (regex_match_func(s1,s2,flags,out,NULL,NULL)==0?1:0)
char *regex_substitute (char *str,char *mat,char *sub,char *flags);

#ifdef __cplusplus
}
#endif

#endif
