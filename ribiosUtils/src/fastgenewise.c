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
/** @file fastgenewise.c
    @brief Module to speed up genewise by shortening introns.
    This version works with genewise that was modified so that coordinates
    dislay 7 digits instead of the original 6 digits.
    Module prefix fg_
*/
#include <unistd.h>
#include <ctype.h>
#include "log.h"
#include "format.h"
#include "linestream.h"
#include "rofutil.h"
#include "sequtil.h"
#include "biosdefs.h" // for GENEWISE_PROG_DIR
#include "fastgenewise.h"

///maximum number of amino acids missing
#define MAX_MISSING 3
/// ???
#define INTRON_LEN 100

static Array cutPos; // of int
static Array cutLen; // of int
static Texta seqFrags;
static char *line;
static int useStrand = 0;
static char *gTmpDir = NULL;

static int correctCoordinate (int coord) {
  int i, len;

  len = 0;
  for (i=arrayMax (cutLen)-1;i>=0;i--) {
    if (arru (cutLen,i,int) >= coord) {
      if (i < arrayMax (cutLen) - 1)
        len = coord - arru (cutLen,i+1,int);
      else
        len = coord;
      break;
    }
  }
  len += arru (cutPos,i,int);
  return len;
}

void fg_setTmpDir (char *tmpDir) {
  /**
     Set the directory to write temporary files
     @param[in] tmpDir - the driectory
  */
  gTmpDir = hlr_strdup (tmpDir);
}

static int extractCoordinate (char *line) {
  int i;
  char coo[20];

  for (i=19;i>0;i--) {
    if (!(isdigit (line[i])))
      break;
  }
  strncpy (coo,line+(i+1),10); // coordinates have a max. of 7 digits
  coo[19-i] = '\0';
  return atoi (coo);
}

char *fg_fastgenewise (char *protein_name,char *genomic_name,
                       char *protein_seq,char *genomic_seq,
                       Array coordinates,char *paramString) {
  /**
     Execute fastgenewise
     @param[in] protein_name - name of the protein
     @param[in] genomic_name - name of the genomic sequence
     @param[in] protein_seq - sequence of the protein
     @param[in] genomic_seq - genomic sequence
     @param[in] coordinates - exon coordinates from a Blast pre-screening
     @param[in] paramString - parameters for genewise
  */
  static Stringa buffer = NULL;
  int i, j, k;
  int pid = getpid ();
  Stringa comm;
  char *cp;
  Stringa protein_spec;
  Stringa genomic_spec;
  int max;
  LineStream outLs;
  Stringa genomicSeq;
  Stringa newGenomicSeq;
  Fg_hsp *hp1,*hp2,*hpLast;
  int gTo,gFrom;
  int upperBound;
  FILE *out;
  static Stringa template1 = NULL;
  int outputPhase = 0;
  int applyCorrection;
  int global = 0;

  max = arrayMax (coordinates);
  if (max == 0)
    return NULL;
  if (gTmpDir == NULL)
    die ("fg_fastgenewise: use fg_setTmpDir before this");
  protein_spec = stringCreate (100);
  genomic_spec = stringCreate (100);
  comm = stringCreate (1000);
  stringPrintf (protein_spec,"%s/fg_protein%d.tfa",gTmpDir,pid);
  stringPrintf (genomic_spec,"%s/fg_gene%d.tfa",gTmpDir,pid);
  stringPrintf (comm,"%s/genewise %s %s ",GENEWISE_PROG_DIR,
                string (protein_spec),string (genomic_spec));
  if (strlen (paramString))
    stringAppendf (comm,"%s ",paramString);
  if (strstr (paramString,"global"))
    global = 1;
  // for the protein sequence
  out = hlr_fopenWrite (string (protein_spec));
  su_printSeqFasta (out,protein_name,protein_seq);
  fclose (out);
  cutPos = arrayCreate (10,int);
  cutLen = arrayCreate (10,int);
  hp1 = arrp (coordinates,0,Fg_hsp);
  if (hp1->sl < hp1->sr)
    useStrand = 1; // use the plus strand
  else
    useStrand = -1; // use the minus strand
  seqFrags = textCreate (10);
  genomicSeq = stringCreate (100000);
  stringCpy (genomicSeq, genomic_seq);
  newGenomicSeq = stringCreate (10000);
  upperBound = stringLen (genomicSeq);
  // now we work our way from 3' to 5' end
  if (useStrand == 1) {
    i = max-1;
    while (i>=0) {
      hp1 = arrp (coordinates,i,Fg_hsp);
      if (hp1->sr + INTRON_LEN >= upperBound) // Direct overlap? Short intron? Seq end? Take all of it.
        gTo = upperBound - 1;
      else {
        gTo = upperBound - 1;
        while (gTo > hp1->sr + INTRON_LEN)
          gTo -= 3; // so that introns stay in phase
      }
      if (global && i == max-1 && hp1->qr < strlen (protein_seq)) // something is missing
        gTo = upperBound - 1;
      /* Next, look for the 5' position up to which we want to go. Normally,
         this will be hp1->sl. However, if there is an exon missing, we use
         the full genomic sequence between two identified exons to give
         GeneWise a chance. Reason: problems with Blast which sometimes
         does not report exons that are obviously there.
      */
      gFrom = hp1->sl;
      j = i-1;
      hpLast = arrp (coordinates,i,Fg_hsp);
      hp2 = NULL;
      while (j>=0) {
        hp2 = arrp (coordinates,j,Fg_hsp);
        if (hpLast->ql - hp2->qr > MAX_MISSING) {
          gFrom = hp2->sl;
          hpLast = hp2;
          i--;
        }
        else
          break;
        j--;
      }
      if (gFrom > INTRON_LEN)
        gFrom -= INTRON_LEN;
      else
        gFrom = 0;
      if (global && i==0 && hp2 != NULL && hp2->ql > 1) // something missing
        gFrom = 0;
      if (gTo > gFrom) {
        k = arrayMax (cutPos);
        array (cutPos,k,int) = gFrom;
        array (cutLen,k,int) = gTo - gFrom + 1;
        textAdd (seqFrags,stringCut (genomicSeq,gFrom,gTo - gFrom + 1));
        upperBound = gFrom;
      }
      i--;
    }
  }
  else if (useStrand == -1) {
    i = 0;
    while (i<max) {
      hp1 = arrp (coordinates,i,Fg_hsp);
      if (hp1->sl + INTRON_LEN >= upperBound) // Direct overlap? Short intron? Seq end? Take all of it.
        gTo = upperBound - 1;
      else {
        gTo = upperBound - 1;
        while (gTo > hp1->sl + INTRON_LEN)
          gTo -= 3; // so that introns stay in phase
      }
      if (global && i == 0 && hp1->ql > 1) // something missing
        gTo = upperBound - 1;
      /* Next, look for the 5' position up to which we want to go. Normally,
         this will be hp1->sr. However, if there is an exon missing, we use
         the full genomic sequence between two identified exons to give
         GeneWise a chance. Reason: problems with Blast which sometimes
         does not report exons that are obviously there.
      */
      gFrom = hp1->sr;
      j = i+1;
      hpLast = arrp (coordinates,i,Fg_hsp);
      hp2 = NULL;
      while (j<max) {
        hp2 = arrp (coordinates,j,Fg_hsp);
        if (hp2->ql - hpLast->qr > MAX_MISSING) {
          gFrom = hp2->sr;
          hpLast = hp2;
          i++;
        }
        else
          break;
        j++;
      }
      if (gFrom > INTRON_LEN)
        gFrom -= INTRON_LEN;
      else
        gFrom = 0;
      if (global && i==max-1 &&
          hp2 != NULL && hp2->qr < strlen (protein_seq)) // something missing
        gFrom = 0;
      if (gTo > gFrom) {
        k = arrayMax (cutPos);
        array (cutPos,k,int) = gFrom;
        array (cutLen,k,int) = gTo - gFrom + 1;
        textAdd (seqFrags,stringCut (genomicSeq,gFrom,gTo - gFrom + 1));
        upperBound = gFrom;
      }
      i++;
    }
    stringCat (comm," -trev "); // tell genewise to use the reverse strand only
  }
  // prepare the shortened version of the genomic sequence
  for (i=arrayMax (seqFrags)-1;i>=0;i--)
    stringCat (newGenomicSeq,textItem (seqFrags,i));
  textDestroy (seqFrags);
  out = hlr_fopenWrite (string (genomic_spec));
  su_printSeqFasta (out,genomic_name,string (newGenomicSeq));
  fclose (out);
  // prepare the static buffer for GeneWise output
  stringCreateClear (buffer,100000);
  if (arrayMax (cutPos) > 0) { // we have to edit the genewise output
    // prepare a cumulative cutLen array
    for (i=arrayMax (cutLen)-2;i>=0;i--)
      arru (cutLen,i,int) += arru (cutLen,i+1,int); // use cumulative data
    outLs = ls_createFromPipe (string (comm)); // run GeneWise
    outputPhase = -1;
    applyCorrection = 0;
    stringCreateClear (template1,50);
    while ((line = ls_nextLine (outLs)) != NULL) {
      if (line[0] == '\0') {
        stringCat (buffer,line);
        stringCat (buffer,"\n");
        continue;
      }
      if (outputPhase == 0) { // the alignment part
        if ((cp = strstr (line,"[")) != NULL) { // number at beginning of intron
          if (isdigit (*(cp+1))) {
            sscanf (cp+1,"%d",&j);
            stringPrintf (template1,"%-7d",correctCoordinate (j));
            for (i=0;i<7;i++)
              *(cp+1+i) = string (template1)[i];
          }
        }
        if ((cp = strstr (line,"]")) != NULL) { // number at end of intron
          if (isdigit (*(cp-1))) {
            sscanf (cp-7,"%d",&j);
            stringPrintf (template1,"%7d",correctCoordinate (j));
            for (i=0;i<7;i++)
              *(cp-7+i) = string (template1)[i];
          }
        }
        if (*(line+19) != ' ') { // this is the position where all coordinates end
          if (applyCorrection) {
            j = extractCoordinate (line);
            stringPrintf (template1,"%d",correctCoordinate (j));
            k = 0;
            for (i=stringLen (template1)-1;i>=0;i--) {
              *(line+19-k) = string (template1)[i];
              k++;
            }
            if (useStrand == 1)
              *(line+19-k) = ' ';
            else if (useStrand == -1)
              *(line+19-k) = '-';
          }
          applyCorrection = !applyCorrection;
        }
        stringCat (buffer,line);
      }
      else if (outputPhase == 1) { // the exon coordinates part
        if (sscanf (line,"Gene %d %d",&i,&j) == 2) {
          stringPrintf (template1,"%d %d",
                        correctCoordinate (i),correctCoordinate (j));
          for (i=0;i<=stringLen (template1);i++)
            *(line+5+i) = string (template1)[i];
        }
        if (sscanf (line,"  Exon %d %d", &i, &j) == 2) {
          stringPrintf (template1,"%d %d",
                        correctCoordinate (i),correctCoordinate (j));
          for (i=0;i<=stringLen (template1);i++)
            *(line+7+i) = string (template1)[i];
        }
        stringCat (buffer,line);
      }
      else if (outputPhase == 2) { // the CDS part
        if ((cp = strstr (line,"[")) != NULL) {
          static Stringa add = NULL;

          sscanf (cp+1,"%d:%d",&i,&j);
          cp = strstr (line,"]");
          stringPrintf (template1,"%d:%d%s",
                        correctCoordinate (i),correctCoordinate (j),cp);
          stringCreateOnce (add,100);
          stringCpy (add,line);
          cp = strstr (string (add),"[");
          for (i=0;i<=stringLen (template1);i++)
            *(cp+1+i) = string (template1)[i];
          stringCat (buffer,string (add));
        }
        else
          stringCat (buffer,line);
      }
      else
        stringCat (buffer,line);
      stringCat (buffer,"\n");
      if (strstr (line,"//") || strstr (line,"WWW help"))
        outputPhase++;
    }
  }
  else // just run genewise
    system (string (comm));
  // clean up in TMPDIR
  stringPrintf (comm,"rm %s",string (protein_spec));
  system (string(comm));
  stringPrintf (comm,"rm %s",string (genomic_spec));
  system (string (comm));
  stringDestroy (protein_spec);
  stringDestroy (genomic_spec);
  stringDestroy (comm);
  return string (buffer);
}
