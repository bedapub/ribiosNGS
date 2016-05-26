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
/** @file seqspeclist.h
    @brief Module for handling segmented list files as used in EMBOSS,
    not EMOSS dependent.
    Note: in more complex cases, use the sequenceObject/sequenceContainer.
    Module prefix seqspec_
*/
#ifndef SEQSPECLIST_H
#define SEQSPECLIST_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdlib.h>
#include <stdio.h>

/// the Seqspec object
typedef struct Seqspec_struct {
  char *dbname; //!< uppercase, for searching; NULL if filename
  char *seqname; //!< uppercase, for searching; NULL if filename
  char *dbseqname; //!< dbname:seqname or filename, case preserving
  int begin; //!< first base of sequence is position 1
  int end; //!< -1 (SEQSPEC_END) = up to the end of the sequence
  int revcompF; //!< 1, if reverse complement, 0 else
}*Seqspec;

/// an indicator to mean end of sequence
#define SEQSPEC_END -1

extern Seqspec seqspec_create (void);
extern void seqspec_destroy_func (Seqspec this1);
/// use this macro instead of seqspec_destroy_func
#define seqspec_destroy(this1) (seqspec_destroy_func(this1),this1=NULL)
extern Seqspec seqspec_clone (Seqspec this1);

extern void seqspec_filesOk (int yesNo);
extern int seqspec_IDset (Seqspec this1,char *seqID);
extern int seqspec_split2continous (Seqspec this1);
extern Seqspec seqspec_parseLine (char *line);

extern int seqspec_read (FILE *f,int continueOnError);
extern int seqspec_readList (char *wildseq);
extern void seqspec_iterInit (void);
extern Seqspec seqspec_next (void);
extern void seqspec_print (FILE *f,int minimal);

#ifdef __cplusplus
}
#endif

#endif
