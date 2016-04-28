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
/** @file seqspeclist.c
    @brief Module for handling segmented list files as used in EMBOSS,
    not EMOSS dependent.
    Note: in more complex cases, use the sequenceObject/sequenceContainer.
    Module prefix seqspec_
*/
#include <ctype.h>
#include "log.h"
#include "format.h"
#include "hlrmisc.h"
#include "rofutil.h"
#include "seqspeclist.h"

/// length of a fragment of a GCG type split sequence, e.g. human chromosomes
#define FRAGMENT_LENGTH 100000

static int seqspec_isCommentLine (char *line) {
  if (isBlankStr (line))
    return 1;
  if (*line == '!' || *line == '#')
    return 1;
  return 0;
}

Seqspec seqspec_create (void) {
  /**
     Create an empty Seqspec object
     Postcondition: the user of this routine is responsible for freeing the
                    object returned
     @return the Seqspec object
  */
  Seqspec this1 = (Seqspec)hlr_malloc (sizeof (struct Seqspec_struct));
  this1->dbname = NULL;
  this1->seqname = NULL;
  this1->dbseqname = NULL;
  this1->begin = 0;
  this1->end = 0;
  this1->revcompF = 0;
  return this1;
}

void seqspec_destroy_func (Seqspec this1) {
  /**
     Destroy a Seqspec object.<br>
     Note: do not call in your programs, use seqspec_destroy() instead
     @param[in] this1 - the Seqspec object
  */
  if (this1 != NULL) {
    hlr_free (this1->dbname);
    hlr_free (this1->seqname);
    hlr_free (this1->dbseqname);
    hlr_free (this1);
  }
}

Seqspec seqspec_clone (Seqspec this1) {
  /**
     Create a fully independent copy of the 'this1'
     (including subfields)<br>
     Note: the user of this routine is responsible for freeing the
     object returned
     @param[in] this1 - the Seqspec object
  */
  Seqspec seqspecp = (Seqspec)hlr_malloc (sizeof (struct Seqspec_struct));
  seqspecp->dbname = hlr_strdup0 (this1->dbname);
  seqspecp->seqname = hlr_strdup0 (this1->seqname);
  seqspecp->dbseqname = hlr_strdup0 (this1->dbseqname);
  seqspecp->begin = this1->begin;
  seqspecp->end = this1->end;
  seqspecp->revcompF = this1->revcompF;
  return seqspecp;
}

static int gFilesOk = 0; // 1 if sequence names can be file names, else 0

void seqspec_filesOk (int yesNo) {
  /**
     Sets the flag whether sequence names can be file names.
     @param[in] yesNo - 1 or 0
  */
  gFilesOk = yesNo;
}

int seqspec_IDset (Seqspec this1, char *seqID) {
  /**
     Set ID of 'this1' sequence segment
     @param[in] this1 - the Seqspec object
     @param[in] seqID - name of UNIX file containing a sequence (filename must
                        not contain ':') or sequence identifier in the form
                        db:seq
     @param[out] this1 - dbseqname always set; dbname and seqname only set if
                         database sequence; if 0 returned then dbseqname,
                         dbname, seqname are undefined
     @param[out] seqID - destroyed
     @return 1 if parsed ok, 0 else if seqspec_filesOk(1) is in effect,
             file names are considered ok, else only seqIDs of the form
             db:seq are ok
  */
  char *seqname;
  strReplace (&this1->dbseqname,seqID);
  if (strchr (seqID,':')) {
    toupperStr (seqID); // DBNAME:SEQNAME
    if ((seqname = dbseqDissect (seqID)) == NULL)
      return 0;
    strReplace (&this1->dbname,seqID);
    strReplace (&this1->seqname,seqname);
  }
  else { // assume file name
    if (gFilesOk) {
      hlr_free (this1->dbname);
      hlr_free (this1->seqname);
    }
    else
      return 0;
  }
  return 1;
}

int seqspec_split2continous (Seqspec this1) {
  /**
     If 'this1' sequence segment is of the form 'db:seq_n begin:b end:e'
     it is transformed into 'db:seq begin:b1 end:e1'
     where b1=b+n*100000, e1=e+n*100000
     which means the GCG type split is removed
     @param[in] this1 - the Seqspec object
     @param[out] this1 - the Seqspec object possibly modified
     @return 1 if 'this1' changed, else 0
  */
  static char *seqname = NULL;
  static Stringa dbseqname = NULL;
  int nameLen;
  char *cp;
  int offset;
  int segnum;

  stringCreateClear (dbseqname,40);
  strReplace (&seqname,this1->seqname);
  nameLen = strlen (seqname);
  if (nameLen < 3)
    return 0;
  if ((cp = strrchr(seqname, '_')) == NULL)
    return 0;
  if (*(cp+1) == '\0' || strlen (cp+1) > 2)
    return 0;
  segnum = atoi (cp+1);
  *cp = 0;
  offset = segnum * FRAGMENT_LENGTH;
  this1->begin += offset;
  if (this1->end != 0)
    this1->end += offset;
  stringCpy (dbseqname,this1->dbname);
  stringCat (dbseqname,":");
  stringCat (dbseqname,seqname);
  seqspec_IDset (this1,string (dbseqname));
  return 1;
}

Seqspec seqspec_parseLine (char *line) {
  /**
     Parses line into a sequence segment;
     @param[in] line - something that can be parsed into a Seqspec object
     @return NULL if line could not be parsed, else pointer to a Seqspec object.
             This object is read-only to its users
  */
  static Seqspec seqspec = NULL;
  char *cp;
  char *value;
  int t;
  char c;
  int blankSkip;
  int strandSeen = 0;
  static Stringa s = NULL;

  if (seqspec == NULL)
    seqspec = seqspec_create ();
  stringCreateClear(s,30);
  // remove leading blanks and blanks after ':'
  blankSkip = 1;
  cp = line - 1;
  while ((c = *++cp) != '\0') {
    if (isspace (c)) {
      if (blankSkip)
        continue;
    }
    else if (c == ':')
      blankSkip = 1;
    else
      blankSkip = 0;
    stringCatChar (s,c);
  }
  wordSet (string (s)," \t");
  if ((cp = wordGet ()) == NULL)
    return NULL;
  if (!seqspec_IDset (seqspec,cp))
    return NULL;
  seqspec->begin = 1;
  seqspec->end = SEQSPEC_END;
  seqspec->revcompF = 0;
  // loop over remaining words of line
  while ((cp = wordGet ()) != NULL) {
    if (!(value = dbseqDissect (cp)))
      continue;
    if (strCaseEqual (cp,"begin"))
      seqspec->begin = atoi (value);
    if (strCaseEqual (cp,"end"))
      seqspec->end = atoi (value);
    if (strCaseEqual (cp,"strand"))
      seqspec->revcompF = (*value == '-');
  }
  if (seqspec->begin <= 0)
    return NULL;
  if (seqspec->end != SEQSPEC_END) {
    // treat e.g. begin:30 end:20 as revcomp
    if (seqspec->end < seqspec->begin) {
      if (strandSeen) {
        warn ("seqspec: bad segment syntax at %s",seqspec->dbseqname);
        return NULL;
      }
      t = seqspec->end;
      seqspec->end = seqspec->begin;
      seqspec->begin = t;
      seqspec->revcompF = 1;
    }
  }
  return seqspec;
}

static Array seqspecs = NULL; // of Seqspec

static void seqspecsInitClear (void)  {
  int i;
  if (seqspecs != NULL) {
    i = arrayMax (seqspecs);
    while (i--)
      seqspec_destroy (arru (seqspecs,i,Seqspec));
    arrayClear (seqspecs);
  }
  else
    seqspecs = arrayCreate (10,Seqspec);
}

int seqspec_read (FILE *f,int continueOnError) {
  /**
     Read sequence segments from stream f.<br>
     Postcondition: seqspec_iterInit() can be called
     @param[in] f - the file handle
     @param[in] continueOnError - 1, if errors are to be silently skipped
                                  0 if program should stop on parsing errors
     @return number of sequence segments successfully read
  */
  int lineCnt = 0;
  int linebuflen;
  char *line = 0;
  Seqspec seqspecp;

  seqspecsInitClear ();
  while (getLine (f,&line,&linebuflen)) {
    lineCnt++;
    stripNlGetLength (line);
    if (seqspec_isCommentLine (line))
      continue;
    if ((seqspecp = seqspec_parseLine (line)) != NULL)
      array (seqspecs,arrayMax (seqspecs),Seqspec) = seqspec_clone (seqspecp);
    else
      if (!continueOnError)
        die ("seqspec_read: cannot understand line %d",lineCnt);
  }
  hlr_free (line);
  return arrayMax (seqspecs);
}

static int seqCnt;
static Stringa fileNames = NULL;

static void readList (char *line) {
  // recursively expand; loops are detected
  if (*line == '@') {
    // line+1 is a list file name
    FILE *f = hlr_fopenRead (line+1);
    int linebuflen;
    char *fline = NULL;
    if (strstr (string (fileNames),line+1))
      die ("loop in @file including at file %s",line+1);
    stringCat (fileNames,line+1);
    stringCat (fileNames,"\001"); // a char not contained in any file name
    while (getLine (f,&fline,&linebuflen)) {
      stripNlGetLength (fline);
      readList (fline);
    }
    hlr_free (fline);
    fclose (f);
  }
  else {
    Seqspec seqspec;

    if (!seqspec_isCommentLine (line) &&
        (seqspec = seqspec_parseLine (line)) != NULL) {
      array (seqspecs,arrayMax (seqspecs),Seqspec) = seqspec_clone (seqspec);
      seqCnt++;
    }
  }
}

int seqspec_readList (char *wildseq) {
  /**
     Do EMBOSS-sytle listfile expansion on 'wildseq'.<br>
     Postcondition: seqspec_iterInit() can be called
     @param[in] wildseq - a wildcrd sequence specification
     @return number of sequence-specifing lines recognized
  */
  /*
    in EMBOSS there are two sorts of 'wildcarding':
    (1) thru joker characters (*, ?)
    (2) thru file inclusion (@file)
    this function re-implements the @file resolution
  */
  seqCnt = 0;
  seqspecsInitClear ();
  stringCreateClear (fileNames,100);
  readList (wildseq);
  return seqCnt;
}

static int seqspecs_pos;

void seqspec_iterInit (void) {
  /**
     Initializes the iterator over Seqspec objects
  */
  if (seqspecs == NULL)
    die ("seqspec_IterInit() without seqspec_read() or seqspec_readList()");
  seqspecs_pos = -1;
}

Seqspec seqspec_next (void) {
  /**
     Returns the next Seqspec object
     @return next Seqspec object
  */
  if (++seqspecs_pos >= arrayMax (seqspecs))
    return NULL;
  return arru (seqspecs,seqspecs_pos,Seqspec);
}

void seqspec_print (FILE *f,int minimal) {
  /**
     Print sequence segments on stream f.<br>
     Precondition: seqspec_read() or seqspec_readList()
     @param[in] f - stream open for writing
     @param[in] minimal - if 1, print minimal syntax, 0 for canonical form
  */
  Seqspec seqspecp;

  seqspec_iterInit ();
  while ((seqspecp = seqspec_next ()) != NULL) {
    fputs (seqspecp->dbseqname,f);
    if (!minimal || seqspecp->begin != 1)
      fprintf (f," begin:%d",seqspecp->begin);
    if (seqspecp->end != SEQSPEC_END)
      fprintf (f," end:%d",seqspecp->end);
    if (seqspecp->revcompF)
      fprintf (f," strand:-");
    if (!minimal && !seqspecp->revcompF)
      fprintf (f," strand:+");
    fputc ('\n',f);
  }
}
