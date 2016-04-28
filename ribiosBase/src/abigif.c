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
/** @file abigif.c
    @brief Creating gif file from an abi trace file.
    Module prefix abigif_
*/
#include "format.h"
#include "gif.h"
#include "abigif.h"

/// structure of a TaggedDataRecord
typedef struct {
  char tagname[4]; //!< name of tag
  long tag_number; //!< number of tag
  short data_type; //!< 4=int; 7=float; 10=mm/dd/yy; 11=hh/mm/ss; 18=pString
  short element_length; //!< ength of element
  long number_of_elements; //!< if 4, means data in data_record
  long record_length; //!< if 4, means data in data_record
  long data_record; //!< number of bytes as offset from file start to data
  long cryptic_variable; //!< ???
}TaggedDataRecord;

static char *abi_int_Int4ToChar (long dat) {
  long local;
  static char *ptr = NULL;
  ptr = (char *)hlr_malloc (5);
  local = dat;
  ptr[4] = '\0';
  ptr[3] = (char)local & 0xff;
  ptr[2] = (char)(local >> 8) & 0xff;
  ptr[1] = (char)(local >> 16) & 0xff;
  ptr[0] = (char)(local >> 24) & 0xff;
  return ptr;
}

static void abigif_getMax (AbiGifObj this1,short *data,long number) {
  long c;
  for (c=this1->indexStart;c<this1->indexEnd;c++) {
    if (c < 0 || c >= this1->record_length)
      continue;
    if (this1->maxValue < data[c])
      this1->maxValue = data[c];
  }
}

static short *abigif_read_data (AbiGifObj this1,TaggedDataRecord *data,FILE *in) {
  short *out;
  fseek (in,data->data_record,SEEK_SET);
  out = (short *)hlr_calloc (data->number_of_elements,sizeof (short));
  this1->record_length = data->number_of_elements;
  fread (out,sizeof (short),data->number_of_elements,in);
  abigif_getMax (this1,out,data->number_of_elements);
  return out;
}

static void abigif_setUpColors (AbiGifObj this1) {
  /*
    routing to set up the image colors
    indices:
    0 BG color
    1 A color
    2 T colot
    3 G color
    4 C color
  */
  int i;
  for (i=0;i<5;i++)
    bm_setColor (this1->dat,i,this1->col[i]);
}

static void abigif_indexSetUp (AbiGifObj this1,TaggedDataRecord *peaks,
                               FILE *in) {
  short *p_vals;
  short extra;

  fseek (in,peaks->data_record,SEEK_SET);
  p_vals = (short *)hlr_calloc (peaks->number_of_elements,sizeof (short));
  fread (p_vals,sizeof(short),peaks->number_of_elements,in);
  if (this1->seqStart && this1->seqStart > 0)
    extra = (p_vals[this1->seqStart] - p_vals[this1->seqStart - 1]) / 2;
  else
    extra = 0;
  if (this1->seqStart > 0)
    this1->indexStart = p_vals[this1->seqStart] - extra;
  else
    this1->indexStart = 0;
  if (this1->seqEnd > peaks->number_of_elements)
    extra = 0;
  else
    extra = (p_vals[this1->seqEnd + 1] - p_vals[this1->seqEnd])/ 2;
  if (this1->seqEnd > peaks->number_of_elements)
    this1->indexEnd = peaks->number_of_elements;
  else
    this1->indexEnd = p_vals[this1->seqEnd] + extra;
  abigif_setIndexSegment (this1,this1->indexStart,this1->indexEnd);
  hlr_free(p_vals);
}

static void abigif_coordSetUp(AbiGifObj this1) {
  if (this1->revComp)
    this1->xTrans = gr_ct_create (this1->indexEnd,this1->indexStart,
                                 0,this1->imgWidth);
  else
    this1->xTrans = gr_ct_create (this1->indexStart,this1->indexEnd,
                                 0,this1->imgWidth);
  this1->yTrans = gr_ct_create (0,this1->maxValue,0,this1->imgHeight);
}

static void abigif_colorTrace (AbiGifObj this1,short *data,int colIndex) {
  long c;
  GraphPoint pt;

  gr_setLineWidth (1);
  gr_setColor (colIndex);
  pt.x = gr_ct_toPix (this1->xTrans,0);
  pt.y = 0;
  gr_moveto (&pt);
  for (c=this1->indexStart;c<this1->indexEnd;c++) {
    pt.x = gr_ct_toPix (this1->xTrans,c);
    if (c < 0 || c > this1->record_length)
      pt.y = 0;
    else
      pt.y = gr_ct_toPix (this1->yTrans,data[c]);
    gr_drawto (&pt);
  }
}

AbiGifObj abigif_create (void) {
  /**
     Postcondition: can call other functions of the module
     @return valid AbiGifObj
  */
  AbiGifObj this1;

  this1 = (AbiGifObj)hlr_malloc (sizeof (struct _abigif_object));
  this1->col[0].red = 0;
  this1->col[0].green = 0;
  this1->col[0].blue = 0;
  this1->col[1].red = 0;
  this1->col[1].green = 255;
  this1->col[1].blue = 0;
  this1->col[2].red = 255;
  this1->col[2].green = 0;
  this1->col[2].blue = 0;
  this1->col[3].red = 255;
  this1->col[3].green = 165;
  this1->col[3].blue = 0;
  this1->col[4].red = 173;
  this1->col[4].green = 216;
  this1->col[4].blue = 230;
  this1->imgHeight = 64;
  this1->imgWidth = 512;
  this1->useSeq = 0;
  this1->seqStart = 0;
  this1->seqEnd = 0;
  this1->indexStart = 0;
  this1->indexEnd = 0;
  this1->userInput = 0;
  this1->revComp = 0;
  this1->dat = NULL;
  this1->maxValue = 0;
  this1->xTrans = NULL;
  this1->yTrans = NULL;
  return this1;
}

void abigif_setColor (AbiGifObj this1,BitmapCol color,int index) {
  /**
     Sets background color of gif file.<br>
     Precondition: create AbiGifObj with abigif_create.<br>
     Postcondition: can call other functions of module
     @param[in] this1 = valid AbiGifObj
     @param[in] color - BitmapCol, sets background color of gif file
     @param[in] index - index inot the color table
  */
  if (index < 0 || index > 4)
    return;
  this1->col[index].red = color.red;
  this1->col[index].green = color.green;
  this1->col[index].blue = color.blue;
}

void abigif_setImageSize (AbiGifObj this1,int width,int height) {
  /**
     Sets size of gif file created.<br>
     Precondition: create AbiGifObj with abigif_create.<br>
     Postcondition: can call other functions of module
     @param[in] this1 - valid AbiGifObj
     @param[in] width - image width
     @param[in] height - image height
  */
  this1->imgWidth = width;
  this1->imgHeight = height;
}

void abigif_setSequenceSegment (AbiGifObj this1,int start,int end) {
  /**
     Sets start and end of sequence displayed from trace
     Precondition: create AbiGifObj with abigif_create
     Postcondition: can call other functions of module
     @param[in] this1 - valid AbiGifObj
     @param[in] start - sequence segment start
     @param[in] end - sequence segment end
  */
  this1->seqStart = start;
  this1->seqEnd = end;
  this1->useSeq = 1;
  this1->userInput = 1;
}

void abigif_setIndexSegment (AbiGifObj this1,int start,int end) {
  /**
     Sets start and end of index displayed from trace
     Precondition: create AbiGifObj with abigif_create
     Postcondition: can call other functions of module
     @param[in] this1 - valid AbiGifObj
     @param[in] start - index segment start
     @param[in] end - index segment end
  */
  this1->indexStart = start;
  this1->indexEnd = end;
  this1->useSeq = 0;
  this1->userInput = 1;
}

void abigif_setRevComp (AbiGifObj this1,int revComp)
{
  /**
     Sets reverse complement of trace
     Precondition: create AbiGifObj with abigif_create
     Postcondition: can call other functions of module
     @param[in] this1 - valid AbiGifObj
     @param[in] revComp - 0 no reverse complement;
                          1 reverse complement trace
  */
  this1->revComp = revComp;
}

int abigif_readFromTrace (AbiGifObj this1,FILE *trace) {
  /**
     Creates a gif image based upon set commands or defaults called
     Precondition: create AbiGifObj with abigif_create
                   change all parameters desired with set functions
     Postcondition: can call abigif_getBitmap to retrieve image created
     @param[in] this1 - valid AbiGifObj
     @param[in] trace - FILE pointer to abi trace file (from fopen)
     @return 1 if not valid ABI trace file was read, else 0
  */
  long indStart;
  long count;
  long numberIndices;
  short *A,*T,*G,*C;
  int indTrace;
  int valid;
  char label[5];
  char *order;
  TaggedDataRecord tag,fwo;
  TaggedDataRecord traces[4],*tr;
  TaggedDataRecord peaks;

  fseek (trace,0,SEEK_SET);
  fread (label,sizeof (char),4,trace);
  if (strNEqual (label,"ABIF",4))
    valid = 1;
  else
    return 0;
  A = T = G = C = NULL;
  if (this1->dat)
    bm_destroy(this1->dat);
  this1->dat = bm_create (this1->imgWidth,this1->imgHeight + 20,8);
  abigif_setUpColors (this1);
  gr_initForBitmap (this1->dat);
  if (!this1->userInput)
    abigif_setIndexSegment (this1,10,200);
  fseek (trace,18,SEEK_SET);
  fread (&numberIndices,sizeof (long),1,trace);
  fseek (trace,26,SEEK_SET);
  fread (&indStart,sizeof (long),1,trace);
  fseek (trace,indStart,SEEK_SET);
  indTrace = 0;
  for (count=0;count<numberIndices;count++) {
    fread (&tag,sizeof (TaggedDataRecord),1,trace);
    if (strNEqual ("FWO_",tag.tagname,4))
      memcpy (&fwo,&tag,sizeof (TaggedDataRecord));
    else if (strNEqual ("DATA",tag.tagname,4) &&
             tag.tag_number >= 9 && indTrace < 4) {
      memcpy (&traces[indTrace],&tag,sizeof (TaggedDataRecord));
      indTrace++;
    }
    else if (strNEqual ("PLOC",tag.tagname,4) &&
             tag.tag_number == 1 && this1->useSeq)
      memcpy (&peaks,&tag,sizeof (TaggedDataRecord));
  }
  if (this1->useSeq)
    abigif_indexSetUp (this1,&peaks,trace);
  order = abi_int_Int4ToChar (fwo.data_record);
  if (this1->revComp) {
    for (count=0;count<4;count++) {
      switch (order[count]) {
      case 'A' : order[count] = 'T';
        break;
      case 'T' : order[count] = 'A';
        break;
      case 'G' : order[count] = 'C';
        break;
      case 'C' : order[count] = 'G';
        break;
      }
    }
  }
  tr = traces;
  for (count=0;count<4;count++) {
    switch (order[count]) {
    case 'A' : A = abigif_read_data (this1,tr+count,trace);
      break;
    case 'T' : T = abigif_read_data (this1,tr+count,trace);
      break;
    case 'G' : G = abigif_read_data (this1,tr+count,trace);
      break;
    case 'C' : C = abigif_read_data (this1,tr+count,trace);
    }
  }
  abigif_coordSetUp (this1);
  abigif_colorTrace (this1,A,1);
  abigif_colorTrace (this1,T,2);
  abigif_colorTrace (this1,G,3);
  abigif_colorTrace (this1,C,4);
  hlr_free (order);
  hlr_free (A);
  hlr_free (T);
  hlr_free (G);
  hlr_free (C);
  return valid;
}

void abigif_destroy_func (AbiGifObj this1) {
  /**
     Destroys an AbiGif object
     @param[in] this1 - valid AbiGifObj
  */
  if (this1 == NULL)
    return;
  if (this1->dat)
    bm_destroy (this1->dat);
  hlr_free (this1);
}

BitmapObject abigif_getBitmap (AbiGifObj this1) {
  /**
     Returns a bitmap based upon trace and set commands or defaults.
     The object returned is read-only (the memory is the responsibility
     of the module)
     Precondition: last call to abigif_readFromTrace() returned 1 (success)
     @param[in] this1 - valid AbiGifObj
  */
  return this1->dat;
}
