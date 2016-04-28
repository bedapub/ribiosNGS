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
/** @file fastgenewise.h
    @brief Module to speed up genewise by shortening introns.
    This version works with genewise that was modified so that coordinates
    dislay 7 digits instead of the original 6 digits.
    Module prefix fg_
*/
#ifndef FASTGENEWISE_H
#define FASTGENEWISE_H

#ifdef __cplusplus
extern "C" {
#endif

/// structure to hold information about one HSP
typedef struct {
  int ql; //!< query (top sequence) left
  int qr; //!< query (top sequence) right
  int sl; //!< subject (bottom sequence) left
  int sr; //!< subject (bottom sequence) right
}Fg_hsp;

extern void fg_setTmpDir (char *tmpDir);
extern char *fg_fastgenewise (char *protein_name,char *genomic_name,
                              char *protein_seq,char *genomic_seq,
                              Array coordinates,char *paramString);

#ifdef __cplusplus
}
#endif

#endif
