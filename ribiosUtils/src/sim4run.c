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
/** @file sim4run.c
    @brief Module that can run sim4.
    Module prefix s4r_
*/
#include <unistd.h>
#include "biosdefs.h"
#include "log.h"
#include "format.h"
#include "linestream.h"
#include "sequtil.h"
#include "rofutil.h"
#include "sim4run.h"

static char *gTmpDir = "/tmp";

static Stringa fn1=NULL,fn2=NULL,comm=NULL;

void s4r_setTmpDir (char *tmpDir) {
  /**
     By default the tmp directory is /tmp. It is dangerous if /tmp fills up
     because the operating system depends on it.
     @param[in] tmpDir - the new tmporary directory
  */
  strReplace (&gTmpDir,tmpDir);
}

LineStream s4r_open (char *name1,char *seq1,char *name2,char *seq2,
                     char *params) {
  /**
     Opens a line stream to sim4 output
     @param[in] name1,name2 - names of 2 sequences
     @param[in] seq1,seq2 - the 2 sequences
     @param[in] params - parameters for sim4 e.g. "R=0 A=1"
     @return a LineStream object containing the SIM4 output
             for use in ls_nextLine() or ls_cat();
             use s4r_close() to destroy this LineStream object
  */
  FILE *fp;
  LineStream ls;

  stringCreateOnce (fn1,100);
  stringPrintf (fn1,"%s/%s.fasta.%d",gTmpDir,name1,getpid ());
  fp = hlr_fopenWrite (string (fn1));
  su_printSeqFasta (fp,name1,seq1);
  fclose (fp);
  stringCreateOnce (fn2,100);
  stringPrintf (fn2,"%s/%s.fasta.%d",gTmpDir,name2,getpid ());
  fp = hlr_fopenWrite (string (fn2));
  su_printSeqFasta (fp,name2,seq2);
  fclose (fp);
  stringCreateOnce (comm,100);
  stringPrintf (comm,"%s/sim4 %s %s %s",BIOINFOBIN,
                string (fn1),string (fn2),params);
  ls = ls_createFromPipe (string (comm));
  return ls;
}

void s4r_close_func (LineStream ls) {
  /**
     ls_destroy is called in the macro s4r_close.<br>
     Do NOT call this function in your program; use macro s4r_close () instead.<br>
     Precondition: ls has been read until its end<br>
     Postcondition: if the SIM4 run initiated by s4r_open ()
                    was NOT successfull, s4r_close () will abort
     @param[in] ls - must have been created with s4r_open ()
  */
  int status;

  stringPrintf (comm,"rm %s",string (fn1));
  system (string (comm));
  stringPrintf (comm,"rm %s",string (fn2));
  system (string (comm));
  if ((status = ls_skipStatusGet (ls)) != 0)
    die ("executing cmd: %s, status=%d. Abort. Sorry.",string (comm),status);
}
