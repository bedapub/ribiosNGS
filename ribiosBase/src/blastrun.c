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
/** @file blastrun.c
    @brief Knows how to start the BLAST program and make its output available.
    Module prefix br_
*/
/*
usage:

 I. If the sequences are ready in a fasta format file (say, test1.seq):
    1. call br_init
    1a. perhaps call blastDbList
    2. call br_setQueryFile
    2. call br_open
       ... process result
    3. call br_close
    4. call br_end

    Example 1:

      debugp = 1;
      br_init (debugp,numtheads,NULL);
      isNucQry = 1;
      isNucDB = 1;
      useTBlastx = 0;
      strcpy (dbnames,"em emnew"); // set of databases ok
      strcpy (seqfile,"test1.seq");
      br_setQueryFile (seqfile);
      ls = br_open (isNucQry,isNucDB,useTBlastx,dbname,blastparams);
      ls_cat (ls,"-"); // process result
      br_close (ls);
      br_end ();

II. If the sequences are not in a file yet:
    1. call br_init
       1a. perhaps call blastDbList
    2. call br_addQueryseq
       call br_addQueryseq
       ...
    3. call br_open
       ... process result
    4. call br_close
       4a. call br_clearQueryseqs
       4b. repeat 2, 3 and 4.
    5. call br_end

    Example 2:

      debugp = 1;
      br_init (debugp,numthreads,"/tmp/blastruntest");
      br_addQueryseq ("test1","testdesc1",
                      "TGGGATTTATTCAAGATGATCATGTTAGAAGTCGTTTAATTAGTTCATTAA");
      br_addQueryseq ("test2","testdesc2",
                      "GATGATCTTCCTCCTGACATCAATAGATTTGAAATAACTCTTAGTAATAAA");

      isNucQry = 1;
      isNucDB = 1;
      useTBlastx = 0;
      strcpy (dbname,"em");

      ls = br_open (isNucQry,isNucDB,useTBlastx,dbname,blstparams);
      ls_cat (ls,"-");
      br_close (ls);

      br_clearQueryseqs ();
      br_addQueryseq ("test3","testdesc3",
                      "TGGGATTTATTCAAGATGATCATGTTAGAAGTCGTTTAATTAGTTCATTAA");
      br_addQueryseq ("test4","testdesc4",
                      "GATGATCTTCCTCCTGACATCAATAGATTTGAAATAACTCTTAGTAATAAA");
      useTBlastx = 1;
      ls = br_open (isNucQry,isNucDB,useTBlastx,dbname,blastparams);
      ls_cat (ls,"-");
      br_close (ls);
      br_end ();
*/

#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include "biosdefs.h"
#include "log.h"
#include "rofutil.h"
#include "blastrun.h"

static int gNumThreads;
static Stringa gQuerySeqFn = NULL;
static char *extfilename = NULL;
static char *blastdbLocation = NULL;
static int gDebug;
static Stringa blastcmd;
static char *gBlastPath = BLAST_PROG_DIR;

void br_init (int doDebug,int numthreads,char *tempDir,char *namePrefix) {
  /**
     Initializes the blastrun module for use.<br>
     Postconditions: br_blastDbList, br_addQueryseq, br_end
                     br_setQueryFile can be called
     @param[in] doDebug - 1 to get debugging behaviour
                          (keep intermediate files and print the blast command)
     @param[in] numthreads - number of threads to use in parallel
                             (maximum is BLAST_MAX_NUM_THREADS)
     @param[in] tempDir - NULL or empty string: write temp file in current
                          working directory; name of temp file is
                          "12345_br_seq" (12345 is the process id)<br>
                          else: directory for the temporary files
     @param[in] namePrefix - prepend the prefix to the sequence name, e.g.
                             myapp_12345_br_seq, can be NULL
  */
  /*
  implementation:
  1. set proper environment variables:
     (check biosdefs.h if you are not sure what the values are.)
     BLASTDB
  2. prepare tmp file for buffering query sequences
  */
  FILE *f;

  gDebug = doDebug;
  if (numthreads < 1)
    gNumThreads = 1;
  else if (numthreads > BLAST_MAX_NUM_THREADS) {
    romsg ("br_init: attempt to use more than %d threads",
           BLAST_MAX_NUM_THREADS);
    gNumThreads = BLAST_MAX_NUM_THREADS;
  }
  else
    gNumThreads = numthreads;
  stringCreateOnce (gQuerySeqFn,20);
  stringPrintf (gQuerySeqFn,"%s%s%s%s%d_br_seq",
                (tempDir == NULL || tempDir[0] == '\0') ? "" : tempDir,
                (tempDir == NULL || tempDir[0] == '\0') ? "" : "/",
                (namePrefix == NULL || namePrefix[0] == '\0') ? "" : namePrefix,
                (namePrefix == NULL || namePrefix[0] == '\0') ? "" : "_",
                getpid ());
  extfilename = NULL;
  f = hlr_fopenWrite (string (gQuerySeqFn)); // check whether it can be written
  fclose (f);
}

void br_setBlastPath (char *path) {
  /**
     By default BLAST_PROG_DIR as specified in biosdefs.h is used as the path
     to the blast executables. This function allows to set an alternative path.
     Should be called before br_open ()
  */
  gBlastPath = path;
}

void br_end (void) {
  /**
     Cleanup when finishing use of this module.<br>
     Precondition: br_init();
  */
  /*
  implementation notes:
  function: remove temporary files
            (<tmpprefix>_seq.nuc, <tmpprefix>_seq.pro, <tmpprefix>_res)
            if gDebug is turned off.
  */
  if (gQuerySeqFn == NULL || stringLen (gQuerySeqFn) == 0)
    die ("br_end() without br_init()");
  if (!gDebug)
    unlink (string (gQuerySeqFn));
  stringClear (gQuerySeqFn);
}

void br_addQueryseq (char *seqname,char *descr,char *seq) {
  /**
     Function: add sequence to be used as query in br_run().<br>
     Precondition: br_init() ran ok<br>
     Postcondition: 'seq' will be used as query by br_run()
                    br_clearQueryseq, br_run can be called<br>
     Note: you should call br_clearQueryseq() if you need to
           blast a new set of sequences.<br>
           The type (nuc/prot) of all sequences submitted must match
           the type used in br_run(isNucQry, ...)<br>
     @param[in] seqname - query sequence name
     @param[in] descr - query sequence description
     @param[in] seq - query sequence
  */
  /*
    implementation:
    br_clearQueryseq() will
    remove the temporary sequence files populated by this routine
  */
  FILE *fps;
  int i, length;

  // convert to fasta format
  if (gQuerySeqFn == NULL || stringLen (gQuerySeqFn) == 0)
    die ("br_addQueryseq() without br_init()");
  fps = hlr_fopenAppend (string (gQuerySeqFn));
  fprintf (fps,">%s %s",seqname,descr);
  length = strlen (seq);
  for (i=0;i<length;i++) {
    if (i%50 == 0)
      fputc ('\n',fps);
    fputc (seq[i],fps);
  }
  fputc ('\n',fps);
  fclose (fps);
}

void br_setQueryFile (char *filename) {
  /**
     Alternative to br_addQueryseq(): give the name of a file
     containing query sequence(s) in FASTA format.<br>
     Postcondition: br_run() can be called and will use the
                    query sequenes from 'filename'. This superseeds
                    any query seqs submitted via br_addQueryseq()
                    and stays in effect until a br_setQueryFile(NULL)
     @param[in] filename - user specified sequence file
                           (in fasta format. It has to be prepared in advanced)
                           or NULL to turn off using an external sequence file
  */
  if (gQuerySeqFn == NULL || stringLen (gQuerySeqFn) == 0)
    die ("br_setQueryFile() without br_init()");
  extfilename = filename;
}

void br_clearQueryseqs (void) {
  /**
     Clear the list of seqs added via br_addQueryseq().<br>
     Precondition: br_init(), br_addQueryseq()
  */
  /*
    implementation:
    empty the temporary query sequence file
  */
  FILE *f;

  if (gQuerySeqFn == NULL || stringLen (gQuerySeqFn) == 0)
    die ("br_clearQueryseqs() without br_init()");
  f = hlr_fopenWrite (string (gQuerySeqFn));
  fclose (f);
}

LineStream br_open (int isNucQry,int isNucDB,int useTBlastx,
                    char *dbnames,char *blastparams) {
  /**
     Precondition: br_init(), [ br_addQueryseq() / br_setQueryFile() ]
     @param[in] isNucQry - 1 if query is nucleotide, 0 if query is protein
     @param[in] isNucDB - 1 if subject database(s) are nucleotide,
                          0 if subject database(s) are protein
     @param[in] useTBlastx - only used when isNucDB == 1 and processing a nuc
                             query: 1 --> use tblastx, 0 --> use blastn
     @param[in] dbnames - name(s) of BLAST database(s), separated by blank
     @param[in] blastparams - additional parameters for blast
     @return a LineStream object containing the BLAST output
             for use in ls_nextLine() or ls_cat();
             use br_close() to destroy this LineStream object
  */
  char *blastprog;
  LineStream ls;
  static Stringa dbsWithPath = NULL;
  int i;
  Texta toks;

  char *seqfile = extfilename != NULL ?
    extfilename : string (gQuerySeqFn);
  if (gQuerySeqFn == NULL || stringLen (gQuerySeqFn) == 0)
    die ("br_open() without br_init()");
  if (isNucQry) {
    if (isNucDB) {
      if (useTBlastx)
        blastprog = "tblastx";
      else
        blastprog = "blastn";
    }
    else // AA
      blastprog = "blastx";
  }
  else { // AA
    if (isNucDB)
      blastprog = "tblastn";
    else // AA
      blastprog = "blastp";
  }
  stringCreateClear (dbsWithPath,100);
  toks = textStrtokP (dbnames," \t,");
  for (i=0;i<arrayMax (toks);i++) {
    if (stringLen (dbsWithPath) > 0)
      stringCat (dbsWithPath," ");
    if (strchr (textItem (toks,i),'/') != NULL)
      stringCat (dbsWithPath,textItem (toks,i));
    else
      stringAppendf (dbsWithPath,"%s/%s",BLAST_DB_DIR,textItem (toks,i));
  }
  textDestroy (toks);
  blastcmd = stringCreate (100);
  stringPrintf (blastcmd,"%s/%s -query %s -db '%s' -num_threads %d %s",
                gBlastPath,blastprog,seqfile,string (dbsWithPath),gNumThreads,
                blastparams != NULL ? blastparams : "");
  if (gDebug) {
    romsg ("running %s on '%s'\n",blastprog,dbnames);
    romsg ("running %s\n",string (blastcmd));
  }
  ls = ls_createFromPipe (string (blastcmd));
  if (ls == NULL) {
    perror ("Does the database exist?");
    br_end ();
    die ("executing cmd: %s, Abort.",string (blastcmd));
  }
  if (gDebug)
    romsg ("br_run: ls created from '%s'\n",string (blastcmd));
  return (ls);
}

void br_close_func (LineStream ls) {
  /**
     Do NOT call this function in your program; use macro br_close()
     instead.<br>
     Precondition: ls has been read until its end<br>
     Postcondition: if the BLAST run initiated by br_open()
                    was NOT successfull, br_close() will abort
     @param[in] ls - must have been created with br_open()
  */
  int status;

  if ((status = ls_skipStatusGet (ls)) != 0)
    die ("executing cmd: %s, status=%d. Abort. Sorry.",
         string (blastcmd),status);
}

void br_dbList (void) {
  /**
     Precondition: br_init()
  */
  static Stringa cmd = NULL;

  if (gQuerySeqFn == NULL || stringLen (gQuerySeqFn) == 0)
    die ("br_dbList() without br_init()");

  printf ("nucleotide databases available for BLAST:\n");
  stringCreateOnce (cmd,50);
  stringPrintf (cmd,"cd %s; /bin/ls *.nsq | cut -f1 -d.",blastdbLocation);
  system (string (cmd));
  printf ("\n");
  printf ("protein databases available for BLAST:\n");
  stringPrintf (cmd,"cd %s; /bin/ls *.psq | cut -f1 -d.",blastdbLocation);
  system (string (cmd));
}

void br_dbFormat (char *fn,int isNuc) {
  /**
     Creates a blast database from a fasta formatted file.
     @param[in] fn - name of the fasta file. If the name contains a path,
                     the directory is changed because formatdb will always
                     write in the current directory.<br>
                     The files generated by formatdb will be like the
                     filename + extension in fn plus .n/pin, .n/phr and
                     .n/psq<br>
                     Make sure that BLASTDB is set to the place where the
                     database is located.
     @param[in] isNuc - whether the sequences are nuc or pro
  */
  Stringa comm;
  char *fn1;
  char *cwd;
  char *pos;

  cwd = getcwd (NULL,-1);
  fn1 = hlr_strdup (fn);
  pos = strrchr (fn1,'/');
  if (pos) {
    *pos = '\0';
    chdir (fn1);
  }
  comm = stringCreate (50);
  stringPrintf (comm,"%s/formatdb -p %c -i %s",BLAST_PROG_DIR,isNuc ? 'F' : 'T',
                pos ? pos+1 : fn1);
  hlr_system (string (comm),0);
  stringDestroy (comm);
  chdir (cwd);
  hlr_free (cwd);
}

void br_dbDelete (char *fn) {
  /**
     @param[in] fn - should be the same as in br_dbFormat
  */
  Stringa comm;

  comm = stringCreate (50);
  stringPrintf (comm,"rm %s.*",fn);
  hlr_system (string (comm),0);
  stringDestroy (comm);
}
