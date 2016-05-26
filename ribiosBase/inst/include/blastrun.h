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
/** @file blastrun.h
    @brief Knows how to start the BLAST program and make its output available.
    Module prefix br_
*/
#ifndef BLASTRUN_H
#define BLASTRUN_H

#ifdef __cplusplus
extern "C" {
#endif

#include "linestream.h"

extern void br_init (int doDebug,int numthreads,char *tempDir,char *namePrefix);
extern void br_setBlastPath (char *path);
extern void br_end (void);
extern void br_addQueryseq (char *seqname,char *descr,char *seq);
extern void br_setQueryFile (char *filename);
extern void br_clearQueryseqs (void);
extern LineStream br_open (int isNucQry,int isNucDB,int useTBlastx,
                           char *dbnames,char *blastparams);
extern void br_close_func (LineStream ls);
/// use the following instead of br_close_func
#define br_close(ls) (br_close_func(ls),ls_destroy(ls))
extern void br_dbList (void);
extern void br_dbFormat (char *fn,int isNuc);
extern void br_dbDelete (char *fn);

#ifdef __cplusplus
}
#endif

#endif
