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
/** @file sim4run.h
    @brief Module that can run sim4.
    Module prefix s4r_
*/
#ifndef SIM4RUN_H
#define SIM4RUN_H

#ifdef __cplusplus
extern "C" {
#endif

#include "linestream.h"

extern void s4r_setTmpDir (char *tmpDir);
extern LineStream s4r_open (char *name1,char *seq1,char *name2,char *seq2,
                            char *params);
extern void s4r_close_func (LineStream ls);
/// Call this macro, not s4r_close_func
#define s4r_close(ls) (s4r_close_func(ls),ls_destroy(ls))

#ifdef __cplusplus
}
#endif

#endif
