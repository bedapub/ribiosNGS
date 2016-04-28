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
/** @file sim4parser.c
    @brief Purpose: dissect the output of the SIM4 program.
    Module prefix sim4p_
*/
#include <ctype.h>
#include "log.h"
#include "format.h"
#include "linestream.h"
#include "regularexpression.h"
#include "sim4parser.h"

#undef SIM4PARSER_DEBUG
/// debug level
#define SIM4PARSER_VERB 7
#ifdef SIM4PARSER_DEBUG
/// write debug message if a < SIM4PARSER_VERB
#define DD(a,b) {if (SIM4PARSER_VERB>=a) {printf b;fflush(stdout);}}
#else
/// write debug message if a < SIM4PARSER_VERB
#define DD(a,b)
#endif

/* --------------- begin module sim4parser --------------------
secret: knows how to dissect the output of the SIM4 program

implementation idea:
  first the user of this module registers functions that should
  be called when interesting points in the sim4 output are found.
  Then the user starts the parser, which in turn drives the functions
  the user registered before, feeding these functions with
  the current data (e.g. query name, subject name, ...). The
  program works for paramters A=0,1,3,4.

public functions:
   void simp_init()
   void simp_run(filename)
   int  simp_register_<name>(function); (returns 1 parsing should continue,
                                       else 0 to stop parser)
     where <name> is:
------------------------------------------------------------
name          when called  /
              what delivered
------------------------------------------------------------
begin         for each new pair of sequences  /
              -
input         after names of input files have been read /
              name of input files and length of sequences
exon          after each exon A=0,3,4/
              query start,end, subject start,end, percentid,
exonseq       after each exon sequence /
              exon coordinates, sequence strings, complement flag
intronseq     after each intron sequence /
              intron coordinates, sequence strings, complement flag
fragmentseq   after complete fragment has been read, imediately after exonseq+intronseq
              intron coordinates, sequence strings, complement flag
end           at the end of a sim4 output
------------------------------------------------------------

private functions: see below, declared 'static'
*/

static int (*begin_hook) (void);
static int (*name_hook) (char *file1,int length1,char *file2,
                         char *dbseq,int length2);
static int (*exon_hook) (int qs,int qe,int ss,int se,int pctid,
                         char *contigous,int complement);
static int (*fragmentseq_hook) (char *scale,char *query,char *match,
                                char *subject,int ident,int sim,int qlen,
                                int alen,int introndir,int complement);
static int (*exonseq_hook) (int qs,int qe,int ss,int se,char *q,char *m,char *s,
                            int pctid,int pctsim,int complement);
static int (*intronseq_hook) (int qs,int qe,int ss,int se,
                              char *q,char *m,char *s,int complement);
static int (*end_hook) (void);

static int gComplement;
static Stringa gScale = NULL;
static Stringa gQuery = NULL;
static Stringa gMatch = NULL;
static Stringa gSubject = NULL;
static Array gNumbers = NULL;
static Array gExons = NULL;
/// to indicate status being in exon
#define EXON 1
/// to indicate status being in intron
#define INTRON 2

/// struture to hold an exon or intron
typedef struct _ExonIntron_ {
  int type; //!< exon 1, intron 2
  Stringa query; //!< the top sequence in the output
  Stringa match; //!< the match line
  Stringa subject; //!< the bottom sequence in the output
  int qs; //!< query start
  int qe; //!< query end
  int ss; //!< subject start
  int se; //!< subject end
  int ident; //!< number of identities
  int sim; //!< number of similarities
  int len; //!< length of intron/exon
}*ExonIntron;

/// structure to hold an exon
typedef struct _Exon_ {
  int qs; //!< query start
  int qe; //!< query end
  int ss; //!< subject start
  int se; //!< subject end
  int pctid; //!< percent identity
  char dir[3]; //!< direction of alignment
}Exon;

/// struture to hold numbers
typedef struct _Numbers_ {
  int scale; //!< scale
  int query; //!< position in query
  int subject; //!< position in subject
}Numbers;

static ExonIntron ei_create (void) {
  ExonIntron new1;
  new1 = (ExonIntron)hlr_malloc (sizeof (struct _ExonIntron_));
  new1->type = 0;
  new1->query = stringCreate (101);
  new1->match = stringCreate (101);
  new1->subject = stringCreate (101);
  new1->ident = 0;
  new1->sim = 0;
  new1->len = 0;
  return new1;
}

static void ei_destroyFunc (ExonIntron this1) {
  stringDestroy (this1->query);
  stringDestroy (this1->match);
  stringDestroy (this1->subject);
  hlr_free (this1);
}

static void ei_arrayClear (Array a) {
  int i;
  ExonIntron curr;
  for (i=0;i<arrayMax (a);i++){
    curr= arru (a,i,ExonIntron);
    ei_destroyFunc (curr);
  }
  arrayClear (a);
}

static int pfBegin (Texta items,int p) {
  int goOn = 1;
  gComplement = 0;
  stringClear (gScale);
  stringClear (gQuery);
  stringClear (gMatch);
  stringClear (gSubject);
  arrayClear (gNumbers);
  if (begin_hook != NULL)
    goOn = (*begin_hook) ();
  return goOn;
}

static int pfName (Texta items,int p) {
  static char *f1;
  static int l1;
  int goOn = 1;
  switch (p) {
  case 1:
    f1 = hlr_strdup (textItem (items,1));
    l1 = atoi (textItem (items,2));
    break;
  case 2:
    if (name_hook != NULL)
      goOn = (*name_hook) (f1,l1,textItem (items,1),textItem (items,2),
                           atoi (textItem (items,3)));
    hlr_free(f1);
  }
  return goOn;
}

static int pfComplement (Texta items,int p) {
  gComplement=1;
  return 1;
}

static int pfExon (Texta items,int p) {
  int goOn = 1;
  Exon *curr;
  curr = arrayp (gExons,arrayMax (gExons),Exon);
  curr->qs = atoi (textItem (items,1));
  curr->qe = atoi (textItem (items,2));
  curr->ss = atoi (textItem (items,3));
  curr->se = atoi (textItem (items,4));
  curr->pctid = atoi (textItem (items,5));
  strcpy (curr->dir,arrayMax (items) == 7 ? textItem (items,6) : "");
  if (exon_hook != NULL)
    goOn = (exon_hook) (curr->qs,curr->qe,curr->ss,curr->se,curr->pctid,
                        curr->dir,gComplement);
  return goOn;
}

static int pfEnd (Texta items,int p) {
  int goOn = 1;
  if (end_hook != NULL)
    goOn = (end_hook) ();
  arrayClear (gExons);
  return goOn;
}

static int pfEndBegin (Texta items,int p) {
  int goOn = 1;
  if (end_hook != NULL)
    goOn = (end_hook) ();
  if (!goOn)
    return goOn;
  if (begin_hook != NULL)
    goOn = (end_hook) ();
  return goOn;
}

static int processFragment (void) {
  int goOn=1,i,j,linenum,alreadyInside,introndir=0;
  int alen=0,qlen=0,ident=0,sim=0,subcord,exonident=0,exonsim=0;
  ExonIntron currei;
  Array eis;
  Numbers *cnump;
  // Adjust length of scale
  while (stringLen (gScale) < stringLen (gMatch)) {
    stringCat (gScale," ");
  }
  /* Adjust length of subject line. I am not sure whether this is
     correct, maybe everthing should be adjusted to match line? */
  while (stringLen (gSubject) < stringLen (gMatch)) {
    stringCat (gSubject," ");
  }
  while (stringLen (gQuery) < stringLen (gMatch)) {
    stringCat (gQuery," ");
  }
  if (stringLen (gScale) != stringLen (gQuery) ||
      stringLen (gQuery) != stringLen (gMatch) ||
      stringLen (gMatch) != stringLen (gSubject))
    die ("%s(l.%i): scale=%i, query=%i, match=%i, subject=%i do not have same length",
         __FILE__,__LINE__,stringLen (gScale),stringLen (gQuery),
         stringLen (gMatch),stringLen (gSubject));

  for (i=0;i<stringLen (gScale);i++){
    switch (string (gMatch)[i]) {
    case ' ':
    case '|':
    case '-': // now exon
      alen++;
      if (isalpha (string (gQuery)[i]))
        qlen++;
      if (string (gQuery)[i] == string (gSubject)[i]) {
        ident++;
        sim++;
      }
      if (string (gQuery)[i] == 'N' && strchr ("ACGT",string (gSubject)[i]))
        sim++;
      else if (string (gSubject)[i] == 'N' && strchr ("ACGT",string (gQuery)[i]))
        sim++;
      break;
    case '=': // now intron
    case '.':
      break;
    case '>':
      introndir++;
      break;
    case '<':
      introndir--;
      break;
    default:
      die ("%s(l.%i): unknown character '%c' in match line",
           __FILE__,__LINE__,string (gMatch)[i]);
    }
  }
  eis = arrayCreate (10,ExonIntron);
  array (eis,arrayMax (eis),ExonIntron) = (currei = ei_create ());
  cnump = arrp (gNumbers,0,Numbers);
  currei->qs = cnump->query;
  currei->ss = cnump->subject;
  currei->qe = cnump->query-1;
  currei->se = cnump->subject-1;
  currei->type = EXON;

  DD (7,("XX: length of alignment %i\n",stringLen (gScale)));
  for (i=0;i<stringLen(gScale);i++){
    if (currei->type==INTRON && strchr (" |-",string (gMatch)[i])) {
      // change state to exon
      DD (7,("XX: change state to exon on position %i\n",i));
      array (eis,arrayMax (eis),ExonIntron) = (currei = ei_create ());
      currei->type = EXON;
      currei->qs = arru (eis,arrayMax (eis)-3,ExonIntron)->qe+1;
      // end of last exon
      currei->qe = arru (eis,arrayMax (eis)-3,ExonIntron)->qe;
      if (strchr ("ACGTN",string (gQuery)[i]))
        currei->qe++;
      if (strchr ("ACGTN",string (gSubject)[i]))
        currei->se++;
      // try to find chromosomal coordinate (starting from next line)
      linenum = (i/50)+1;
      j = linenum*50;
      subcord = arrp (gNumbers,linenum,Numbers)->subject;
      currei->ss = subcord;
      alreadyInside = string (gMatch)[j-1] == '.' ? 1 : 0;
      DD (7,("XX: linenum=%i j=%i subcord=%i\n",linenum,j,subcord));
      for (--j;j>=i;j--) {
        if (strchr (".",string (gMatch)[j]) && !alreadyInside) {
          // modified from '=.><' to '.' as only dots indicate coordinate change
          currei->ss = -1;
          break;
        }
        else if (strchr ("ACGTN",string (gSubject)[j]) || alreadyInside) {
          currei->ss--;
        }
      }
      // in case not successful try the current line
      if (currei->ss == -1) {
        linenum = (i/50);
        j = linenum*50;
        subcord = arrp (gNumbers,linenum,Numbers)->subject;
        currei->ss = subcord;
        for (;j<i;j++) {
          if (strchr (".",string (gMatch)[j])) {
            // modified from '=.><' to '.' as only dots indicate coordinate change
            currei->ss = -1;
            break;
          }
          else if (strchr ("ACGTN",string (gSubject)[j])) {
            currei->ss++;
          }
        }
      }
      // in case not successful use coordinates from exon list
      if (currei->ss == -1) {
        Exon *curr;
        for (j=0;j<arrayMax (gExons);j++) {
          curr = arrp (gExons,j,Exon);
          if (curr->qs == currei->qs) {
            currei->ss=curr->ss;
            break;
          }
        }
      }
      currei->se = currei->ss;
      if (currei->ss != -1) { // transfer to intron
        arru (eis,arrayMax (eis)-2,ExonIntron)->se = currei->ss-1;
      }
    }
    else if (strchr (" |-",string (gMatch)[i])) { // still exon
      if (strchr ("ACGTN",string (gQuery)[i])) {
        currei->qe++;
      }
      if (strchr ("ACGTN",string (gSubject)[i])) {
        currei->se++;
      }
    }
    else if (currei->type == EXON && strchr ("=.><",string (gMatch)[i])) {
      // change state to intron
      DD (7,("XX: change state to intron on position %i\n",i));
      array (eis,arrayMax (eis),ExonIntron) = (currei = ei_create ());
      currei->type = INTRON;
      currei->ss = arru (eis,arrayMax (eis)-2,ExonIntron)->se+1;
      // end of last exon
      currei->se = -1;
      currei->qs = -1;
      currei->qe = -1;
    }
    else if (strchr ("=.><",string (gMatch)[i])) { // still intron
    }
    else {
      die ("%s(l.%i): unknown character '%c' in match string",
           __FILE__,__LINE__,string (gMatch)[i]);
    }
    stringAppendf (currei->query,"%c",string (gQuery)[i]);
    stringAppendf (currei->match,"%c",string (gMatch)[i]);
    stringAppendf (currei->subject,"%c",string (gSubject)[i]);
  }

  DD (7,("XX:Found %i exons and introns\n",arrayMax (eis)));
  if (intronseq_hook || exonseq_hook) {
    for (i=0;i<arrayMax (eis);i++) {
      currei = arru (eis,i,ExonIntron);
      if (currei->type == EXON&&exonseq_hook) {
        exonident=0;
        exonsim=0;
        for (j=0;j<stringLen (currei->match);j++) {
          if (string (currei->query)[j] == string (currei->subject)[j]) {
            exonident++;
            exonsim++;
          }
          if (string (currei->query)[j] == 'N' &&
              strchr ("ACGTN",string (currei->subject)[j]))
            exonsim++;
          else if (string (currei->subject)[j] == 'N' &&
                   strchr ("ACGTN",string (currei->query)[j]))
            exonsim++;
        }
        DD (7,("ZZ: exonident=%i exonsim=%i stringLen(gScale)=%i\n",
               exonident,exonsim,stringLen (gScale)));
        goOn = exonseq_hook (currei->qs,currei->qe,currei->ss,currei->se,
                             string (currei->query),string (currei->match),
                             string (currei->subject),
                             exonident*100/stringLen (currei->subject),
                             exonsim*100/stringLen (currei->subject),
                             gComplement);
      }
      else if (currei->type == INTRON && intronseq_hook)
        goOn = intronseq_hook (currei->qs,currei->qe,currei->ss,currei->se,
                               string (currei->query),string (currei->match),
                               string (currei->subject),gComplement);
      if (!goOn)
        return goOn;
    }
  }

  if (fragmentseq_hook != NULL)
    goOn = fragmentseq_hook (string (gScale),string (gQuery),string (gMatch),
                             string(gSubject),
                             ident*100/alen,sim*100/alen,qlen,alen,introndir/6,
                             gComplement);
  ei_arrayClear (eis);
  arrayDestroy (eis);
  return goOn;
}

static int pfSeq (Texta items,int p) {
  int goOn = 1;
  static Numbers *currp;
  if (p == 1)
    DD (7,("YY: Scale number=%i Len(gScale)=%i\n",
           atoi (textItem (items,1)),stringLen (gScale)));
  switch (p) {
  case 1: // scale
    if (atoi (textItem (items,1)) < stringLen (gScale)) {
      goOn = processFragment ();
      stringClear (gScale);
      stringClear (gQuery);
      stringClear (gMatch);
      stringClear (gSubject);
      arrayClear (gNumbers);
    }
    stringAppendf (gScale,"%s",textItem (items,2));
    currp = arrayp (gNumbers,arrayMax (gNumbers),Numbers);
    currp->scale = atoi (textItem (items,1));
    break;
  case 2: // query
    stringAppendf (gQuery,"%s",textItem (items,2));
    currp->query = atoi (textItem (items,1));
    break;
  case 3: // match
    stringAppendf (gMatch,"%s",textItem (items,1));
    break;
  case 4: // subject
    stringAppendf (gSubject,"%s",textItem (items,2));
    currp->subject = atoi (textItem (items,1));
    break;
  case 5: // done
    goOn = processFragment ();
    if (goOn)
      return goOn;
    goOn = pfEnd (items,0);
    break;
  default:
    die ("%s(l.%i): Unknown parameter %i",__FILE__,__LINE__,p);
  }
  return goOn;
}

/// at beginning
#define stBegin 1
/// reading name1
#define stName1 2
/// reading name 2
#define stName2 3
/// after name2
#define stAfterName2 4
/// determining whether complement or not
#define stComplement 5
/// complement or not is known
#define stEmptyAfterComplement 6
/// reading exon
#define stExon 7
/// exon is read
#define stAfterExon 8
/// ?
#define stEmptyAfterExon 9
/// reading sequence scale
#define stSeqScale 10
/// reading query sequence (sequene 1)
#define stSeqQuery 11
/// reading match sequence
#define stSeqMatch 12
/// reading subject sequence (sequence 2)
#define stSeqSubject 13
/// ?
#define stEmptyAfterSeq 14
/// at end
#define stEnd 1000

/// holds status, pattern to be matched and functions to be called
static struct {
  int s1; //!< status1
  int s2; //!< status2
  char *m; //!< pattern
  int (*f1) (Texta items,int p); //!< function1
  int p1; //!< parameter1
  int (*f2) (Texta items,int p); //!< function2
  int p2; //!< parameter2
}pfuna[] = {
  {stBegin,stBegin,"^$",NULL,0,NULL,0},
  {stBegin,stName1,"^seq1 = ([^,]+), ([0-9]+) bp$",pfBegin,0,pfName,1},
  {stName1,stName2,"^seq2 = ([^ ]+) \\(([^\\)]+)\\), ([0-9]+) bp$",NULL,0,pfName,2},
  {stName2,stAfterName2,"^$",NULL,0,NULL,0},
  {stAfterName2,stAfterName2,"^$",NULL,0,NULL,0},
  {stAfterName2,stEnd,NULL,pfEnd,0,NULL,0},
  {stAfterName2,stName1,"^seq1 = ([^,]+), ([0-9]+) bp$",pfEndBegin,0,pfName,1},
  {stAfterName2,stComplement,"^\\(complement\\)$",NULL,0,pfComplement,0},
  {stAfterName2,stExon,"^([0-9]+)-([0-9]+)  \\(([0-9]+)-([0-9]+)\\)   ([0-9]+)% ([-=><]{2})$",NULL,0,pfExon,1},
  {stAfterName2,stAfterExon,"^([0-9]+)-([0-9]+)  \\(([0-9]+)-([0-9]+)\\)   ([0-9]+)%$",NULL,0,pfExon,3},
  {stComplement,stExon,"^([0-9]+)-([0-9]+)  \\(([0-9]+)-([0-9]+)\\)   ([0-9]+)% ([-=><]{2})$",NULL,0,pfExon,1},
  {stComplement,stAfterExon,"^([0-9]+)-([0-9]+)  \\(([0-9]+)-([0-9]+)\\)   ([0-9]+)%$",NULL,0,pfExon,3},
  {stComplement,stEmptyAfterComplement,"^$",NULL,0,NULL,0},
  {stEmptyAfterComplement,stSeqScale,"^ +([0-9]+) ([ .:]*)$",NULL,0,pfSeq,1},
  {stEmptyAfterComplement,stEnd,"^$",pfEnd,0,NULL,0},
  {stExon,stExon,"^([0-9]+)-([0-9]+)  \\(([0-9]+)-([0-9]+)\\)   ([0-9]+)% ([-=><]{2})$",NULL,0,pfExon,2},
  {stExon,stAfterExon,"^([0-9]+)-([0-9]+)  \\(([0-9]+)-([0-9]+)\\)   ([0-9]+)%$",NULL,0,pfExon,3},
  {stAfterExon,stEnd,NULL,pfEnd,0,NULL,0},
  {stAfterExon,stEmptyAfterExon,"^$",NULL,0,NULL,0},
  {stEmptyAfterExon,stName1,"^seq1 = ([^,]+), ([0-9]+) bp$",pfEndBegin,0,pfName,1},
  {stEmptyAfterExon,stSeqScale,"^ +([0-9]+) ([ .:]*)$",NULL,0,pfSeq,1},
  {stSeqScale,stSeqQuery,"^ +([0-9]+) ([ACGTUMRWSYKVHDBXN.~ ]+)$",NULL,0,pfSeq,2},
  {stSeqQuery,stSeqMatch,"^        ([-| =.<>]+)$",NULL,0,pfSeq,3},
  {stSeqMatch,stSeqSubject,"^ *([0-9]+) ([ACGTUMRWSYKVHDBXN.~ ]+)$",NULL,0,pfSeq,4},
  {stSeqSubject,stEmptyAfterSeq,"^$",NULL,0,NULL,0},
  {stEmptyAfterSeq,stSeqScale,"^ +([0-9]+) ([ .:]*)$",NULL,0,pfSeq,1},
  {stEmptyAfterSeq,stBegin,"^$",pfSeq,5,NULL,0},
  {stEmptyAfterSeq,stEnd,NULL,pfSeq,5,NULL,0},
  {stEnd,stName1,"^seq1 = ([^,]+), ([0-9]+) bp$",pfBegin,0,pfName,1},
  {stEnd,stEnd,NULL,NULL,0,NULL,0},
};

void sim4p_register_begin (int (*f) (void)) {
  /**
     Registers the function f that is called at the beginning of each query
     f has no arguments
  */
  begin_hook = f;
}

void sim4p_register_name (int (*f) (char *file1,int length1,char *file2,
                                    char *dbseq,int length2)) {
  /**
     Registers the function f that is called after the name and length of the
     input files have been read
  */
  /*
    file1 - name of input file 1
    length - length of first sequence
    file2 - name of input file 2
    dbseq - dbseq of second file (first field in fasta formatted file)
    length2 - length of second sequence
  */
  name_hook = f;
}
void sim4p_register_exon (int (*f)(int qs,int qe,int ss,int se,int pctid,
                                   char *contigous,int complement)) {
  /**
     Registers the function f that is called after each exon has been read
     (those that appear in A=0 output)
  */
  /*
    qs - start coordinate of the query
    qe - end coordinate of the query
    ss - start coordinate of the query
    se - end coordinate of the query
    contigous - the two character symbol found at the end of each line
                (->,<-,==)
    complement - 1 the reverse complement of the sequence was used for the
                 alignment, 0 otherwise
  */
  exon_hook = f;
}

void sim4p_register_fragmentseq (int (*f)(char *scale,char *query,char *match,
                                          char *subject,int ident,int sim,
                                          int qlen,int alen,
                                          int introndir,int complement)) {
  /**
     Registers the function f that is called after each fragment has been read
     (fragments are defined by increasing numbers in the scale line of the
     alignment output A=3)
  */
  /*
    scale - pointer to the scale line (without any linebreaks)
    query - pointer to the query line (without any linebreaks)
    match - pointer to the match line (without any linebreaks)
    subject - pointer to the subject line (without any linebreaks)
    ident - percent identity over complete fragment
    sim - percent similarity over complete fragment (N matches ACGT)
    qlen - length of aligned query (exluding gaps)
    alen - length of alignment (including gaps)
    introndir - main direction of introns (match character >=+1, <=-1,
                introndir=sum/6)
    complement - 1 the reverse complement of the sequence was used for the
                 alignment, 0 otherwise
  */
  fragmentseq_hook = f;
}

void sim4p_register_exonseq (int (*f) (int qs,int qe,int ss,int se,char *q,
                                       char *m,char *s,int pctid,int pctsim,
                                       int complement)) {
  /**
     Registers the function f that is called for each exon after the fragment
     has been read immediately before the fragmentseq function is called
  */
  /*
    qs - start coordinate of the query
    qe - end coordinate of the query
    ss - start coordinate of the query, as good as could be derived from
         alignment
    se - end coordinate of the query, as good as could be derived from alignment
    q - pointer to the query line (without any linebreaks)
    m - pointer to the match line (without any linebreaks)
    s - pointer to the subject line (without any linebreaks)
    pctid - percent identity for this exon
    complement - 1 the reverse complement of the sequence was used for the
                 alignment, 0 otherwise
  */
  exonseq_hook = f;
}

void sim4p_register_intronseq (int (*f) (int qs,int qe,int ss,int se,char *q,
                                         char *m,char *s,int complement)) {
  /**
     Registers the function f that is called for each intron (between two exons)
     immediately before the fragmentseq function is called
  */
  /*
    qs - start coordinate of the query
    qe - end coordinate of the query
    ss - start coordinate of the query, as good as could be derived from
         alignment
    se - end coordinate of the query, as good as could be derived from alignment
    q - pointer to the query line (without any linebreaks)
    m - pointer to the match line (without any linebreaks)
    s - pointer to the subject line (without any linebreaks)
    complement - 1 the reverse complement of the sequence was used for the
                 alignment, 0 otherwise
  */
  intronseq_hook = f;
}

void sim4p_register_end (int (*f) (void)) {
  /**
     Register fuction f that is called after each query has been processed
     it has no arguments
  */
  end_hook = f;
}

void sim4p_init (void) {
  /**
     Initialize the sim4 parser<br>
     Postcondition: sim4p_register_? functions and sim4p_run can be called
  */
  begin_hook = NULL;
  name_hook = NULL;
  exon_hook = NULL;
  fragmentseq_hook = NULL;
  exonseq_hook = NULL;
  intronseq_hook = NULL;
  end_hook = NULL;
}

void sim4p_run (LineStream ls) {
  /**
     Parses the output of the sim4 program and calls user defined functions<br>
     Postcondition: the functions registered have been called
     @param[in] ls - input line stream with sim4 output
  */
  char *line;
  int state=stBegin,i,goOn=1;
  Texta items= textCreate (10);

  if (ls == NULL)
    return;
  gScale = stringCreate (1000);
  gQuery = stringCreate (1000);
  gMatch = stringCreate (1000);
  gSubject = stringCreate (1000);
  gNumbers = arrayCreate (10,Numbers);
  gExons = arrayCreate (10,Exon);
  while (goOn && (line = ls_nextLine (ls))) {
    DD (7,("<%s\n",line));
    for (i=0;i<NUMELE (pfuna);i++) {
      if (state == pfuna[i].s1 &&
          regex_match (line,pfuna[i].m,NULL,items)){
        DD (17,("compare '%s' with pattern '%s' -> ",line,pfuna[i].m));
        DD (17,("match\n"));
        if (pfuna[i].f1 != NULL)
          pfuna[i].f1 (NULL,pfuna[i].p1);
        if (pfuna[i].f2 != NULL)
          goOn = pfuna[i].f2 (items,pfuna[i].p2);
        state = pfuna[i].s2;
        break;
      }
    }
    if (i==NUMELE (pfuna)) {
      die ("%s(l.%i): no pattern in state %i matched line %i: '%s'\n",
           __FILE__,__LINE__,state,ls_lineCountGet (ls),line);
    }
  }
  if (goOn && line == NULL) {
    for (i=0;i<NUMELE (pfuna);i++) {
      if (state == pfuna[i].s1 && pfuna[i].m == NULL) {
        if (pfuna[i].f1 != NULL)
          pfuna[i].f1 (NULL,pfuna[i].p1);
        break;
      }
    }
    if (i==NUMELE (pfuna)) {
      die ("%s(l.%i): EOF should not appear in state %i.\n",
           __FILE__,__LINE__,state);
    }
  }
  stringDestroy (gScale);
  stringDestroy (gQuery);
  stringDestroy (gMatch);
  stringDestroy (gSubject);
  arrayDestroy (gNumbers);
  arrayDestroy (gExons);
}
