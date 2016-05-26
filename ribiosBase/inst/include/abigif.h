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
/** @file abigif.h
    @brief Creating gif file from an abi trace file.
    Module prefix abigif_
*/
#ifndef ABIGIF_H
#define ABIGIF_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include "graphics.h"
#include "bitmap.h"

// do not refer to anything inside of this struct for your programs

/// structure of the abigif object
typedef struct _abigif_object {
  BitmapObject dat; //!< to hold the bitmap
  BitmapCol col[5]; /**< all of the colors
                         0 background color
                         1 color of the A trace
                         2 color of the T trace
                         3 color of the G trace
                         4 color of the C trace */
  int imgHeight; //!< height of the image
  int imgWidth; //!< width of the image
  int useSeq; //!< flag to use seq segment or index
  int seqStart; //!< start of sequence to use
  int seqEnd; //!< end of sequence to use
  int indexStart; //!< start of index (of trace values)
  int indexEnd; //!< end of index (of trace values)
  int userInput; /**< used to indicate user input values for seq or
                      index segment (and not from trace file) */
  int revComp; /**< used to indication direction and
                    complementarity 0 -> uncomplemented
                    1 -> reverse complement */
  long record_length; //!< how much data (bytes ???) in record
  short maxValue; /**< used to create the graph, this will
                       be the maximum user coordinate */
  GraphCoordTrans xTrans; //!< coordinate translation for x
  GraphCoordTrans yTrans; //!< coordinate translation for y
}*AbiGifObj;

/// background color
#define ABIGIF_BG_COLOR 0
/// color for nucleotide A
#define ABIGIF_A_COLOR 1
/// color for nucleotide C
#define ABIGIF_T_COLOR 2
/// color for nucleotide G
#define ABIGIF_G_COLOR 3
/// color for nucleotide T
#define ABIGIF_C_COLOR 4

/// convenience
#define abigif_setBGColor(a,b) abigif_setColor(a,b,ABIGIF_BG_COLOR)
/// convenience
#define abigif_setAColor(a,b) abigif_setColor(a,b,ABIGIF_A_COLOR)
/// convenience
#define abigif_setTColor(a,b) abigif_setColor(a,b,ABIGIF_T_COLOR)
/// convenience
#define abigif_setGColor(a,b) abigif_setColor(a,b,ABIGIF_G_COLOR)
/// convenience
#define abigif_setCColor(a,b) abigif_setColor(a,b,ABIGIF_C_COLOR)

extern AbiGifObj abigif_create (void);
extern void abigif_setColor (AbiGifObj this1,BitmapCol color,int index);
extern void abigif_setImageSize (AbiGifObj this1,int width,int height);
extern void abigif_setSequenceSegment (AbiGifObj this1,int start,int end);
extern void abigif_setIndexSegment (AbiGifObj this1,int start,int end);
extern void abigif_setRevComp (AbiGifObj this1,int revComp);

extern int abigif_readFromTrace (AbiGifObj this1,FILE *trace);
extern BitmapObject abigif_getBitmap (AbiGifObj this1);

extern void abigif_destroy_func (AbiGifObj this1);
/// use this instead of abigif_destroy_func()
#define abigif_destroy(m) (abigif_destroy_func(m),m=NULL)

#ifdef __cplusplus
}
#endif

#endif
