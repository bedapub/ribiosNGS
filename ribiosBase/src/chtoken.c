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
/** @file chtoken.c
    @brief Dissect a C header file into tokens.
    Module prefix cht_
*/
#include <ctype.h>
#include "log.h"
#include "format.h"
#include "hlrmisc.h"
#include "rofutil.h"
#include "chtoken.h"

char cht_deEscapeChar (char *s,int *len) {
  /**
     Decode C language character representation into actual character
     @param[in] s - string starting with escaped or unescaped
                    char, e.g. "abc", "\nabc", "\10abc"
     @param[out] len - length of s used, e.g. if s is "\112a", *len is 4;
                       0 if there is a problem with the input
     @return first char of s decoded (binary)
  */
  char c;
  int code = 0;
  char *text;
  if (*s != '\\') {
    if (*s == '\0')
      die ("cht_deEscapeChar: empty");
    *len = 1;
    return *s;
  }
  s++;
  if (*s == '\0')
    die ("cht_deEscapeChar: empty after \\");
  *len = 2;
  c = *s;
  if (c == 'n')
    return '\n';
  if (c == 't')
    return '\t';
  if (c == 'r')
    return '\r';
  if (c == 'b')
    return '\b';
  if (c == 'f')
    return '\f';
  if (c == '\\')
    return '\\';
  if (c == '\'')
    return '\'';
  if (!isdigit (c))
    return c;
  // is it an octal integer?
  code = strtol (s,&text,8);
  if (s == text) {
    warn ("not an octal escape sequence %s",s-1);
    *len = 0;
  }
  else if (code < 0 || code > 255) {
    warn ("invalid escape sequence %s",s-1);
    *len = 0;
  }
  else
    *len = text - s + 1;
  return (char)code;
}

char *cht_deEscapeString (char *s) {
  /**
     Interpret the char specifications embedded in a string;
     null character in the input is forbidden
     according to C language rules
     @param[in] s - null-terminated string
     @return null-terminated string; this memory may be modified
             but not free'd nor realloc'd by the caller of this routine.
             It will stay stable until the next call to this function.
  */
  int len;
  char *cp = s;
  static Stringa r = NULL;
  stringCreateClear (r,100);
  while (*cp != '\0') {
    stringCatChar (r,cht_deEscapeChar (cp,&len));
    cp += len;
  }
  return string (r);
}

/* mini module: read file with continuation line breaks
   hides continuation line breaks; shows file as one long string
   with long lines
*/

char *cht_fileReadContCat (char *fileName) {
  /**
     @param[in] fileName - '-' means stdin
     @return contents of file, which continuation lines concatenated;
             memory managed by this routine; stable until next call
             to this routine.
  */
  static Stringa s = NULL;
  FILE *f = hlr_fopenRead (fileName);
  char previousC = '\0';
  int c;
  stringCreateClear (s,4000);
  while ((c = getc (f)) != EOF) {
    if (c == '\0')
      die ("NUL char in input file. abort.");
    if (previousC == '\\' && c == '\n')
      stringChop (s,1); // discard '\\' and do not add '\n'
    else
      stringCatChar (s,c);
    previousC = c;
  }
  fclose (f);
  return string (s);
}

// module chtoken (cht_): dissect C header text into tokens

static char *t_cp;
static char *t_text;

void cht_init (char *fileName) {
  /**
     @param[in] fileName - <br>
     Postcondition: cht_get() can be called
  */
  t_text = cht_fileReadContCat (fileName);
  t_cp = t_text;
}

char *cht_context (void) {
  /**
     Precondition: t_cp - current position; t_text - beginning of text.<br>
     @return a few chars before and the current position, nicely formatted
             for error reporting
  */
  static int ccntMax = 50;
  static int nlcntMax = 3;
  static Stringa r = NULL;
  int ccnt = 0;
  int nlcnt = 0;
  char *cp = t_cp+1;
  stringCreateClear (r,200);

  // get left context
  while (--cp > t_text) {
    if (!isspace (*cp))
      if (++ccnt > ccntMax)
        break;
    if (*cp == '\n')
      if (++nlcnt > nlcntMax)
        break;
  }
  stringCpy (r,"\n");
  stringNCat (r,cp,t_cp - cp + 1);
  // insert a position marker
  stringCat (r," <==***==>\n");
  // append right context
  if (*t_cp == '\0')
    return string (r);
  ccnt = 0;
  nlcnt = 0;
  cp = t_cp;
  while (*++cp != '\0') {
    if (!isspace (*cp))
      if (++ccnt > ccntMax)
        break;
    if (*cp == '\n')
      if (++nlcnt > nlcntMax)
        break;
  }
  stringNCat (r,t_cp+1,cp - t_cp - 2);
  stringCat (r,"\n");
  return string (r);
}

static int tryThisWord (char *word) {
  /**
     Check if word is current token, and eat it if yes.<br>
     Precondition: t_cp  -- points to begin of compare text.<br>
     Postcondition: if 1 is returned, t_cp is advance by the length of 'word'
     @return 1 if t_cp is on word and word is properly ended, else 0
  */
  int len = strlen (word);
  char c; // first char after word
  if (strncmp (t_cp,word,len))
    return 0;
  c = *(t_cp+len);
  if (isalnum (c) || c == '_') // part of an identifier
    return 0;
  t_cp += len;
  return 1;
}

static int tryIdentifier (char *s) {
  /**
     If s begins with an identifier, return its length;
     else return 0
  */
  char c = *s;
  char *cp = s;
  if (!isalpha (c) && c != '_')
    return 0;
  while ((c = *++cp) != '\0')
    if (! (isalnum (c) || c == '_'))
      break;
  return cp - s;
}

int cht_getAll (Stringa value) {
  /**
     Returns the type and value of the current token.<br>
     Precondition: cht_init().<br>
     Postcondition: next call to this routine returns next token
     @param[in] value - Stringa
     @param[out] value - changed only in case of CHT_COMMENT1, CHT_COMMENT2,
                         T_*CONST, CHT_IDENTIFIER and CHT_NULL
                         in case of CHT_CHARCONST the code of the char is given
                         as a string integer, e.g. '\100' --> "64".
                         CHT_STRINGCONST value is the uninterpreted value, i.e.
                         \n etc are left as is. Only flanking double quotes are
                         removed.
                         CHT_NULL is accompanied by an emtpy string.
     @return 0 if end of input, else one of the token constants T_*
  */
  char c;
  char *cp;
  int len;
  int i;

  t_cp--;
  while (isspace (c = *++t_cp) && c != '\n'); // skip whitespace except NL
  if (c == '\0')
    return 0; // end of input
  t_cp++;
  if (c == '\n')
    return CHT_NL;
  if (c == ';')
    return CHT_SEMICOLON;
  if (c == '=')
    return CHT_EQUAL;
  if (c == '{')
    return CHT_CURLYOPEN;
  if (c == '}')
    return CHT_CURLYCLOSE;
  if (c == '[')
    return CHT_BRACKETOPEN;
  if (c == ']')
    return CHT_BRACKETCLOSE;
  if (c == '(')
    return CHT_PARENSOPEN;
  if (c == ')')
    return CHT_PARENSCLOSE;
  if (c == '*')
    return CHT_STAR;
  if (c == ',')
    return CHT_COMMA;
  if (c == '/' && *t_cp != '*' && *t_cp != '/')
    return CHT_SLASH;
  // none of these simple cases, restore pointer
  t_cp--;
  if (tryThisWord ("#define"))
    return CHT_DEFINE;
  if (tryThisWord ("#ifdef"))
    return CHT_IFDEF;
  if (tryThisWord ("#ifndef"))
    return CHT_IFNDEF;
  if (tryThisWord ("#endif"))
    return CHT_ENDIF;
  if (tryThisWord ("static"))
    return CHT_STATIC;
  if (tryThisWord ("char"))
    return CHT_CHAR;
  if (tryThisWord ("NULL")) {
    stringClear (value);
    return CHT_NULL;
  }
  if (tryThisWord ("int"))
    return CHT_INT;
  // is it a /* */ comment?
  if (strNEqual (t_cp,"/*",2)) {
    cp = t_cp+1;
    while ((c = *++cp) != '\0') {
      if (c == '/' && *(cp+1) == '*')
        die ("%scannot handle nested comments. sorry. abort.",cht_context ());
      if (c == '*' && *(cp+1) == '/') {
        stringNCpy (value,t_cp,cp - t_cp + 1 + 1);
        t_cp = cp+2;
        return CHT_COMMENT1;
      }
    }
    die ("comment start without comment end. abort.");
  }
  // is it a // comment?
  if (strNEqual (t_cp,"//",2)) {
    cp = t_cp+1;
    while ((c = *++cp) != '\0') {
      if (c == '\n') {
        stringNCpy (value,t_cp,cp - t_cp);
        t_cp = cp;
        return CHT_COMMENT2;
      }
    }
    die ("comment start without comment end. abort.");
  }
  // is it a char constant?
  if (c == '\'') {
    t_cp++;
    c = cht_deEscapeChar (t_cp,&len);
    if (len == 0)
      die ("%sinvalid char spec",cht_context ());
    stringClear (value);
    stringCatInt (value,(int)c);
    if (*(t_cp+len) != '\'')
      die ("%sunterminated char constant",cht_context ());
    t_cp += (len + 1);
    return CHT_CHARCONST;
  }
  // is it a string constant?
  if (c == '"') {
    int escapeMode = 0;
    cp = t_cp;
    while ((c = *++cp) != '\0') {
      if (c == '\n')
        die ("%sunescaped newline in string constant",cht_context ());
      // run up to first non-escaped double quote
      if (escapeMode) { // ignore exactly one char after a backslash
        escapeMode = 0;
        continue;
      }
      if (c == '\\') {
        escapeMode = 1;
        continue;
      }
      if (c == '"') {
        stringNCpy (value,t_cp+1,cp-t_cp-1);
        t_cp += cp-t_cp+1;
        return CHT_STRINGCONST;
      }
    }
    die ("%sunterminated string constant",cht_context ());
  }
  // is it an integer constant?
  if (sscanf (t_cp,"%i%n",&i,&len) == 1) {
    stringClear (value);
    stringCatInt (value,i);
    t_cp += len;
    return CHT_INTCONST;
  }
  // is it an identifier?
  if ((len = tryIdentifier (t_cp)) != 0) {
    stringNCpy (value,t_cp,len);
    t_cp += len;
    return CHT_IDENTIFIER;
  }
  die ("\n------------- unrecognized token --------------------------\n%s",
       cht_context ());
  return 0; // keep compiler happy
}

int cht_get (Stringa value) {
  /**
     Same as cht_getAll(), but suppresses comments
     @param[in] value - Stringa
     @param[out] value - changed only in case of CHT_COMMENT1, CHT_COMMENT2,
                         T_*CONST, CHT_IDENTIFIER and CHT_NULL
                         in case of CHT_CHARCONST the code of the char is given
                         as a string integer, e.g. '\100' --> "64".
                         CHT_STRINGCONST value is the uninterpreted value, i.e.
                         \n etc are left as is. Only flanking double quotes are
                         removed.
                         CHT_NULL is accompanied by an emtpy string.
     @return 0 if end of input, else one of the token constants T_*
  */
  int token;

  while ((token = cht_getAll (value)) != 0) {
    if (token != CHT_COMMENT1 && token != CHT_COMMENT2)
      break;
  }
  return token;
}
