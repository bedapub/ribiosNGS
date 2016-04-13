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
/** @file regularexpression.c
    @brief Module to handle regular expressions.
    Examples for regular expressions:
    "^seq1 = ([^,]+), ([0-9]+) bp$".
    Module prefix regex_
*/
#include <sys/types.h>
#include <regex.h>
#include "log.h"
#include "regularexpression.h"

/// ???
#define SUBEXP_MAX 31

#undef RGX_DEBUG
/// debug level
#define RGX_VERB 9
#ifdef RGX_DEBUG
/// debug message
#define DD(a,b) {if (RGX_VERB>=a) {printf("%s(l.%i): ",__FILE__,__LINE__);printf b;fflush(stdout);fflush(stderr);}}
#else
/// debug message
#define DD(a,b)
#endif

int regex_match_func (char *s1,char *s2,char *flags,Texta out,
                      Array begin,Array end){
  /**
     Match 'Extended Regular Expression' s2 in string s1. The 'Extended Regular
     Expression' syntax is described in the regcomp(5) man page and complies
     with the ISO/IEC 9945-2: 1993 standard.
     Subexpressions are stored in 'out' and supported flags
     are i (for 'case insensitive match').<br>
     Postcondition: regex_match_func(s1,NULL,...) will use s2 from this call
     Note: use macro regex_match for convenience, it returns 1 on success
           (match), 0 otherwise.
     @param[in] s1 - string to be matched
     @param[in] s2 - regular expression, NULL means: use last one
     @param[in] flags - flags: 'i', NULL means 'case sensitive'
     @param[in] out - Array of char*, may be NULL
     @param[in] begin - Array of int, may be NULL
     @param[in] end - Array of int, may be NULL
     @param[out] out - Texta with subexpressions
     @param[out] begin - offsets in s1 of matched (sub-)expressions
     @param[out] end - offsets in s1 of the end of matched (sub-)expressions
     @return 0 if expression was found, other numbers in case of
             no match or failure (see regcomp(3G)).
  */
  regmatch_t pm[SUBEXP_MAX+1];
  int compflag=0,result,i,j;
  static regex_t re;
  static int initialized=0;
  char *pos;
  compflag=REG_EXTENDED;
  if (flags && strchr (flags,'i')) // ignore case
    compflag |= REG_ICASE;
  if (s2 != NULL) {
    if (initialized) {
      regfree (&re);
    }
    if (regcomp (&re,s2,compflag) != 0) {
      die ("%s(l.%i): regex_match(): failed to compile regular expression '%s'",
           __FILE__,__LINE__,s2);
    }
    else {
      initialized=1;
    }
  }
  // search in string
  result=regexec (&re,s1,SUBEXP_MAX+1,&pm[0],0);
  if (result==0) { // fill array of subexpressions
    if (out != NULL)
      textClear (out);
    if (begin)
      arrayClear (begin);
    if (end)
      arrayClear (end);
    for (i=0;i<=SUBEXP_MAX;i++) {
      if (pm[i].rm_so!=-1) {
        if (begin)
          array (begin,i,int) = (int)pm[i].rm_so;
        if (end)
          array (end,i,int) = (int)pm[i].rm_eo;
        if (out) {
          array(out,i,char*) = (pos = (char *)hlr_calloc (pm[i].rm_eo-pm[i].rm_so+1,sizeof(char)));
          for (j=pm[i].rm_so;j<pm[i].rm_eo;j++) {
            *(pos++) = *(s1+j);
          }
        }
      }
    }
  }
  return result;
}

char *regex_substitute (char *str,char *mat,char *sub,char *flags) {
  /**
     Substitute mat in str by sub
     @param[in] str - string on which function works
     @param[in] mat - pattern to be replaced
     @param[in] sub - string to be inserted instead of pattern
     @param[in] flags - 'i' case independent, 'g' global
     @return string with substitutions, stable until the next call
  */
  static Stringa input=NULL; // store copy of str, in case str==string(result)
  static Stringa result=NULL;
  static Array begin=NULL;
  static Array end=NULL;
  char *pos;
  int count=0;
  // initialize
  if (begin == NULL)
    begin = arrayCreate (10,int);
  if (end == NULL)
    end = arrayCreate (10,int);
  stringCreateOnce (input,100);
  stringCpy (input,str);
  stringCreateClear (result,100);
  regex_match_func ("",mat,(char *)(strchr(flags,'i')?"i":""),NULL,NULL,NULL);
  pos=string (input);
  // find match expression and replace 1 or more times
  while (0==regex_match_func (pos,mat,(char *)(strchr(flags,'i')?"i":""),
                              NULL,begin,end) &&
         (count<1 || strchr (flags,'g'))) {
    count++;
    stringNCat (result,pos,arru (begin,0,int)); // before match
    stringCat (result,sub);
    pos = pos+arru (end,0,int);
  }
  stringCat (result,pos);
  return string (result);
}

#if 0
---------------------- for convenience, the regcomp(5) man page ---------

REGCOMP(5)                                                          REGCOMP(5)

NAME
     regcomp - X/Open regular expressions definition and interface

DESCRIPTION
     Note:  Two versions of regular expressions are supported:

     o    the historical Simple Regular Expressions, which provide backward
          compatibility, but which will be withdrawn from a future issue of
          this document set

     o    the improved internationalised version that complies with the
          ISO/IEC 9945-2: 1993 standard.

     The first (historical) version is described as part of the regexp
     function in the regexp(5) man page. The second (improved) version is
     described in this man page.

     Regular Expressions (REs) provide a mechanism to select specific strings
     from a set of character strings.

     Regular expressions are a context-independent syntax that can represent a
     wide variety of character sets and character set orderings, where these
     character sets are interpreted according to the current locale. While
     many regular expressions can be interpreted differently depending on the
     current locale, many features, such as character class expressions,
     provide for contextual invariance across locales.

     The Basic Regular Expression (BRE) notation and construction rules in bre
     apply to most utilities supporting regular expressions.  Some utilities,
     instead, support the Extended Regular Expressions (ERE) described in ere;
     any exceptions for both cases are noted in the descriptions of the
     specific utilities using regular expressions. Both BREs and EREs are
     supported by the Regular Expression Matching interface in the regcmp(),
     regexec() and related functions.

Regular Expression Definitions
     For the purposes of this section, the following definitions apply:

     entire regular expression

     The concatenated set of one or more BREs or EREs that make up the pattern
     specified for string selection.

     matched

          A sequence of zero or more characters is said to be matched by a BRE
          or ERE when the characters in the sequence correspond to a sequence
          of characters defined by the pattern.

          Matching is based on the bit pattern used for encoding the
          character, not on the graphic representation of the character. This
          means that if a character set contains two or more encodings for a
          graphic symbol, or if the strings searched contain text encoded in
          more than one codeset, no attempt is made to search for any other
          representation of the encoded symbol. If that is required, the user
          can specify equivalence classes containing all variations of the
          desired graphic symbol.

          The search for a matching sequence starts at the beginning of a
          string and stops when the first sequence matching the expression is
          found, where first is defined to mean ``begins earliest in the
          string''. If the pattern permits a variable number of matching
          characters and thus there is more than one such sequence starting at
          that point, the longest such sequence will be matched. For example:
          the BRE bb* matches the second to fourth characters of abbbc, and
          the ERE (wee|week)(knights|night) matches all ten characters of
          weeknights.

          Consistent with the whole match being the longest of the leftmost
          matches, each subpattern, from left to right, matches the longest
          possible string. For this purpose, a null string is considered to be
          longer than no match at all. For example, matching the BRE \(.*\).*
          against abcdef, the subexpression (\1) is abcdef, and matching the
          BRE \(a*\)* against bc, the subexpression (\1) is the null string.

          It is possible to determine what strings correspond to
          subexpressions by recursively applying the leftmost longest rule to
          each subexpression, but only with the proviso that the overall match
          is leftmost longest. For example, matching \(ac*\)c*d[ac]*\1 against
          acdacaaa matches acdacaaa (with \1=a); simply matching the longest
          match for \(ac*\) would yield \1=ac, but the overall match would be
          smaller (acdac). Conceptually, the implementation must examine every
          possible match and among those that yield the leftmost longest total
          matches, pick the one that does the longest match for the leftmost
          subexpression and so on. Note that this means that matching by
          subexpressions is context-dependent: a subexpression within a larger
          RE may match a different string from the one it would match as an
          independent RE, and two instances of the same subexpression within
          the same larger RE may match different lengths even in similar
          sequences of characters. For example, in the ERE (a.*b)(a.*b), the
          two identical subexpressions would match four and six characters,
          respectively, of accbaccccb.

          When a multi-character collating element in a bracket expression is
          involved, the longest sequence will be measured in characters
          consumed from the string to be matched; that is, the collating
          element counts not as one element, but as the number of characters
          it matches.

     BRE (ERE) matching a single character

          A BRE or ERE that matches either a single character or a single
          collating element.

          Only a BRE or ERE of this type that includes a bracket expression
          can match a collating element.

          The definition of single character has been expanded to include also
          collating elements consisting of two or more characters; this
          expansion is applicable only when a bracket expression is included
          in the BRE or ERE.  An example of such a collating element may be
          the Dutch ij, which collates as a y.  In some encodings, a ligature
          ``i with j'' exists as a character and would represent a single-
          character collating element. In another encoding, no such ligature
          exists, and the two-character sequence ij is defined as a multi-
          character collating element. Outside brackets, the ij is treated as
          a two-character RE and matches the same characters in a string.
          Historically, a bracket expression only matched a single character.
          If, however, the bracket expression defines, for example, a range
          that includes ij, then this particular bracket expression will also
          match a sequence of the two characters i and j in the string.

     BRE (ERE) matching multiple characters

          A BRE or ERE that matches a concatenation of single characters or
          collating elements.

     invalid

          This section uses the term invalid for certain constructs or
          conditions.  Invalid REs will cause the utility or function using
          the RE to generate an error condition. When invalid is not used,
          violations of the specified syntax or semantics for REs produce
          undefined results: this may entail an error, enabling an extended
          syntax for that RE, or using the construct in error as literal
          characters to be matched. For example, the BRE construct \{1,2,3\}
          does not comply with the grammar. A portable application cannot rely
          on it producing an error nor matching the literal characters
          \{1,2,3\}.

Regular Expression General Requirements
     The requirements in this section apply to both basic and extended regular
     expressions.

     The use of regular expressions is generally associated with text
     processing.  REs (BREs and EREs) operate on text strings; that is, zero
     or more characters followed by an end-of-string delimiter (typically
     NUL).  Some utilities employing regular expressions limit the processing
     to lines; that is, zero or more characters followed by a newline
     character.  In the regular expression processing described in this
     document, the newline character is regarded as an ordinary character and
     both a period and a non-matching list can match one. The individual man
     pages specify within the individual descriptions of those standard
     utilities employing regular expressions whether they permit matching of
     newline characters; if not stated otherwise, the use of literal newline
     characters or any escape sequence equivalent produces undefined results.

     Those utilities (like grep) that do not allow newline characters to match
     are responsible for eliminating any newline character from strings before
     matching against the RE. The regcomp() function (see regcomp(3G)),
     however, can provide support for such processing without violating the
     rules of this section.

     The interfaces specified in this document set do not permit the inclusion
     of a NUL character in an RE or in the string to be matched.  If during
     the operation of a standard utility a NUL is included in the text
     designated to be matched, that NUL may designate the end of the text
     string for the purposes of matching.

     When a standard utility or function that uses regular expressions
     specifies that pattern matching will be performed without regard to the
     case (upper- or lower-) of either data or patterns, then when each
     character in the string is matched against the pattern, not only the
     character, but also its case counterpart (if any), will be matched. This
     definition of case-insensitive processing is intended to allow matching
     of multi-character collating elements as well as characters. For
     instance, as each character in the string is matched using both its
     cases, the RE [[.Ch.]] when matched against the string char, is in
     reality matched against ch, Ch, cH and CH.

     The implementation will support any regular expression that does not
     exceed 256 bytes in length.

Basic Regular Expressions
     BREs Matching a Single Character or Collating Element

          A BRE ordinary character, a special character preceded by a
          backslash or a period matches a single character. A bracket
          expression matches a single character or a single collating element.

     BRE Ordinary Characters

          An ordinary character is a BRE that matches itself: any character in
          the supported character set, except for the BRE special characters
          listed in brespec.

          The interpretation of an ordinary character preceded by a backslash
          (\) is undefined, except for:

          1.  the characters ), (, { and }

          2.  the digits 1 to 9 inclusive

          3.  a character inside a bracket expression.

     BRE Special Characters

          A BRE special character has special properties in certain contexts.
          Outside those contexts, or when preceded by a backslash, such a
          character will be a BRE that matches the special character itself.
          The BRE special characters and the contexts in which they have their
          special meaning are:

     .[\  The period, left-bracket and backslash is special except when used
          in a bracket expression. An expression containing a [ that is not
          preceded by a backslash and is not part of a bracket expression
          produces undefined results.

     *    The asterisk is special except when used:

          o   in a bracket expression

          o   as the first character of an entire BRE (after an initial ^,
              if any)

          o   as the first character of a subexpression (after an initial ^,
              if any).

     ^    The circumflex is special when used:

          o   as an anchor

          o   as the first character of a bracket expression.

     $    The dollar sign is special when used as an anchor.

     Periods in BREs

          A period (.), when used outside a bracket expression, is a BRE that
          matches any character in the supported character set except NUL.


     RE Bracket Expression

     A bracket expression (an expression enclosed in square brackets, [ ]) is
     an RE that matches a single collating element contained in the non-empty
     set of collating elements represented by the bracket expression.

     The following rules and definitions apply to bracket expressions:


     1.   A bracket expression is either a matching list expression or a non-
          matching list expression. It consists of one or more expressions:
          collating elements, collating symbols, equivalence classes,
          character classes or range expressions. Portable applications must
          not use range expressions, even though all implementations support
          them. The right-bracket (]) loses its special meaning and represents
          itself in a bracket expression if it occurs first in the list (after
          an initial circumflex (^), if any). Otherwise, it terminates the
          bracket expression, unless it appears in a collating symbol (such as
          [.].]) or is the ending right-bracket for a collating symbol,
          equivalence class or character class. The special characters:

          . * [ \

          (period, asterisk, left-bracket and backslash, respectively) lose
          their special meaning within a bracket expression.

          The character sequences:

          [. [= [:

          (left-bracket followed by a period, equals-sign or colon) are
          special inside a bracket expression and are used to delimit
          collating symbols, equivalence class expressions and character class
          expressions. These symbols must be followed by a valid expression
          and the matching terminating sequence .], =] or :], as described in
          the following items.

     2.   A matching list expression specifies a list that matches any one of
          the expressions represented in the list. The first character in the
          list must not be the circumflex. For example, [abc] is an RE that
          matches any of the characters a, b or c.

     3.   A non-matching list expression begins with a circumflex (^), and
          specifies a list that matches any character or collating element
          except for the expressions represented in the list after the leading
          circumflex. For example, [^abc] is an RE that matches any character
          or collating element except the characters a, b or c. The circumflex
          will have this special meaning only when it occurs first in the
          list, immediately following the left-bracket.

     4.   A collating symbol is a collating element enclosed within bracket-
          period ([. .]) delimiters. Collating elements are defined as
          described in colltbl(1M). Multi-character collating elements must be
          represented as collating symbols when it is necessary to distinguish
          them from a list of the individual characters that make up the
          multi-character collating element.  For example, if the string ch is
          a collating element in the current collation sequence with the
          associated collating symbol <ch>, the expression [[.ch.]] will be
          treated as an RE matching the character sequence ch, while [ch] will
          be treated as an RE matching c or h.  Collating symbols will be
          recognised only inside bracket expressions. This implies that the RE
          [[.ch.]]*c matches the first to fifth character in the string
          chchch. If the string is not a collating element in the current
          collating sequence definition, or if the collating element has no
          characters associated with it (for example, see the symbol <HIGH> in
          the example collation definition shown in colltbl(1M)), the symbol
          will be treated as an invalid expression.

     5.   An equivalence class expression represents the set of collating
          elements belonging to an equivalence class, as described in
          colltbl(1M).  Only primary equivalence classes will be recognised.
          The class is expressed by enclosing any one of the collating
          elements in the equivalence class within bracket-equal ([= =])
          delimiters.  For example, if a, agrave and acircumflex belong to the
          same equivalence class, then [=a=]b], [[=agrave=]b] and
          [[=acircumflex=]b] will each be equivalent to [aagraveacircumflexb].
          If the collating element does not belong to an equivalence class,
          the equivalence class expression will be treated as a collating
          symbol.

     6.   A character class expression represents the set of characters
          belonging to a character class, as defined in the LC_CTYPE category
          in the current locale. All character classes specified in the
          current locale will be recognised. A character class expression is
          expressed as a character class name enclosed within bracket-colon
          ([: :]) delimiters.

          The following character class expressions are supported in all
          locales:

          The following character class expressions are supported in all
          locales:

               [:alnum:]     [:cntrl:]     [:lower:]     [:space:]
               [:alpha:]     [:digit:]     [:print:]     [:upper:]
               [:blank:]     [:graph:]     [:punct:]     [:xdigit:]

          In addition, character class expressions of the form:

               [:name:]

          are recognised in those locales where the name keyword has been
          given a charclass  definition in the LC_CTYPE category.

     7.   A range expression represents the set of collating elements that
          fall between two elements in the current collation sequence,
          inclusively. It is expressed as the starting point and the ending
          point separated by a hyphen (-).

          Range expressions must not be used in portable applications because
          their behaviour is dependent on the collating sequence. Ranges will
          be treated according to the current collating sequence, and include
          such characters that fall within the range based on that collating
          sequence, regardless of character values. This, however, means that
          the interpretation will differ depending on collating sequence. If,
          for instance, one collating sequence defines aumlat as a variant of
          a, while another defines it as a letter following z, then the
          expression [aumlat-z]  is valid in the first language and invalid in
          the second.

          In the following, all examples assume the collation sequence
          specified for the POSIX locale, unless another collation sequence is
          specifically defined.

          The starting range point and the ending range point must be a
          collating element or collating symbol. An equivalence class
          expression used as a starting or ending point of a range expression
          produces unspecified results. An equivalence class can be used
          portably within a bracket expression, but only outside the range.
          For example, the unspecified expression [[=e=]-f] should be given as
          [[=e=]e-f]. The ending range point must collate equal to or higher
          than the starting range point; otherwise, the expression will be
          treated as invalid. The order used is the order in which the
          collating elements are specified in the current collation
          definition. One-to-many mappings (see the description of LC_COLLATE
          in locale(1)) will not be performed. For example, assuming that the
          character eszet is is placed in the collation sequence after r and
          s, but before t and that it maps to the sequence ss for collation
          purposes, then the expression [r-s] matches only r and s, but the
          expression [s-t] matches s, eszet ot t.

          The interpretation of range expressions where the ending range point
          is also the starting range point of a subsequent range expression
          (for instance [a-m-o]) is undefined.

          The hyphen character will be treated as itself if it occurs first
          (after an initial ^, if any) or last in the list, or as an ending
          range point in a range expression. As examples, the expressions [-
          ac] and [ac-] are equivalent and match any of the characters a, c or
          -; [^-ac] and [^ac-] are equivalent and match any characters except
          a, c or -; the expression [%- -] matches any of the characters
          between % and - inclusive; the expression [- -@] matches any of the
          characters between - and @ inclusive; and the expression [a- -@] is
          invalid, because the letter a follows the symbol - in the POSIX
          locale. To use a hyphen as the starting range point, it must either
          come first in the bracket expression or be specified as a collating
          symbol, for example: [][.-.]-0], which matches either a right
          bracket or any character or collating element that collates between
          hyphen and 0, inclusive.

          If a bracket expression must specify both - and ], the ] must be
          placed first (after the ^, if any) and the - last within the bracket
          expression.

          BREs Matching Multiple Characters

          The following rules can be used to construct BREs matching multiple
          characters from BREs matching a single character:

     1.   The concatenation of BREs matches the concatenation of the strings
          matched by each component of the BRE.

     2.   A subexpression  can be defined within a BRE by enclosing it between
          the character pairs \( and \) . Such a subexpression matches
          whatever it would have matched without the \( and \), except that
          anchoring within subexpressions is optional behaviour.
          Subexpressions can be arbitrarily nested.

     3.   The back-reference  expression \n matches the same (possibly empty)
          string of characters as was matched by a subexpression enclosed
          between \( and \) preceding the \n. The character n must be a digit
          from 1 to 9 inclusive, nth subexpression (the one that begins with
          the nth \( and ends with the corresponding paired \)).  The
          expression is invalid if less than n subexpressions precede the \n.
          For example, the expression ^\(.*\)\1$ matches a line consisting of
          two adjacent appearances of the same string, and the expression
          \(a\)*\1 fails to match a. The limit of nine back-references to
          subexpressions in the RE is based on the use of a single digit
          identifier.  This does not imply that only nine subexpressions are
          allowed in REs. The following is a valid BRE with ten
          subexpressions:

     \(\(\(ab\)*c\)*d\)\(ef\)*\(gh\)\{2\}\(ij\)*\(kl\)*\(mn\)*\(op\)*\(qr\)*

     4.   When a BRE matching a single character, a subexpression or a back-
          reference is followed by the special character asterisk (*),
          together with that asterisk it matches what zero or more consecutive
          occurrences of the BRE would match. For example, [ab]* and [ab][ab]
          are equivalent when matching the string ab.

     5.   When a BRE matching a single character, a subexpression or a back-
          reference is followed by an interval expression of the format \{m\},
          \{m,\} or \{m,n\}, together with that interval expression it matches
          what repeated consecutive occurrences of the BRE would match. The
          values of m and n will be decimal integers in the range 0 <= m <= n
          <= RE_DUP_MAX, where m specifies the exact or minimum number of
          occurrences and n specifies the maximum number of occurrences. The
          expression \{m\} matches exactly m occurrences of the preceding BRE,
          \{m,\} matches at least m occurrences and \{m,n\} matches any number
          of occurrences between m and n, inclusive.

          For example, in the string abababccccccd the BRE c\{3\} is matched
          by characters seven to nine, the BRE \(ab\)\{4,\} is not matched at
          all and the BRE c\{1,3\}d is matched by characters ten to thirteen.

     The behaviour of multiple adjacent duplication symbols (* and intervals)
     produces undefined results.

     BRE Precedence

          The order of precedence is as shown in the following table:

          BRE Precedence (from high to low)

               collation-related bracket symbols   [= =]  [: :]  [. .]

               escaped characters                  \<special character>

               bracket expression                  []

               subexpressions/back-references      \(\)\n

               single-character-BRE duplication    *\{m,n\}

               concatenation

               anchoring                           ^ $

     BRE Expression Anchoring

          A BRE can be limited to matching strings that begin or end a line;
          this is called anchoring.  The circumflex and dollar sign special
          characters will be considered BRE anchors in the following contexts:

     1.   A circumflex (^) is an anchor when used as the first character of an
          entire BRE. The implementation may treat circumflex as an anchor
          when used as the first character of a subexpression. The circumflex
          will anchor the expression (or optionally subexpression) to the
          beginning of a string; only sequences starting at the first
          character of a string will be matched by the BRE. For example, the
          BRE ^ab matches ab in the string abcdef, but fails to match in the
          string cdefab. The BRE \(^ab\) may match the former string.  A
          portable BRE must escape a leading circumflex in a subexpression to
          match a literal circumflex.

     2.   A dollar sign ($) is an anchor when used as the last character of an
          entire BRE. The implementation may treat a dollar sign as an anchor
          when used as the last character of a subexpression.  The dollar sign
          will anchor the expression (or optionally subexpression) to the end
          of the string being matched; the dollar sign can be said to match
          the end-of-string following the last character.

     3.   A BRE anchored by both ^ and $ matches only an entire string. For
          example, the BRE ^abcdef$ matches strings consisting only of abcdef.

     Extended Regular Expressions

          The extended regular expression (ERE) notation and construction
          rules will apply to utilities defined as using extended regular
          expressions; any exceptions to the following rules are noted in the
          descriptions of the specific utilities using EREs.

     EREs Matching a Single Character or Collating Element

          An ERE ordinary character, a special character preceded by a
          backslash or a period matches a single character.  A bracket
          expression matches a single character or a single collating element.
          An ERE matching a single character enclosed in parentheses matches
          the same as the ERE without parentheses would have matched.

     ERE Ordinary Characters

          An ordinary character  is an ERE that matches itself.  An ordinary
          character is any character in the supported character set, except
          for the ERE special characters listed in erespec.  The
          interpretation of an ordinary character preceded by a backslash (\)
          is undefined.

     ERE Special Characters

          An ERE special character has special properties in certain contexts.
          Outside those contexts, or when preceded by a backslash, such a
          character is an ERE that matches the special character itself.  The
          extended regular expression special characters and the contexts in
          which they have their special meaning are:


     . [ \ (
          The period, left-bracket, backslash and left-parenthesis are special
          except when used in a bracket expression.  Outside a bracket
          expression, a left-parenthesis immediately followed by a right-
          parenthesis produces undefined results.

     )    The right-parenthesis is special when matched with a preceding
          left-parenthesis, both outside a bracket expression.

     * + ? {
          The asterisk, plus-sign, question-mark and left-brace are special
          except when used in a bracket expression.  Any of the following uses
          produce undefined results:

               if these characters appear first in an ERE, or immediately
               following a vertical-line, circumflex or left-parenthesis

               if a left-brace is not part of a valid interval expression.

     |    The vertical-line is special except when used in a bracket
          expression.  A vertical-line appearing first or last in an ERE, or
          immediately following a vertical-line or a left-parenthesis, or
          immediately preceding a right-parenthesis, produces undefined
          results.

     ^    The circumflex is special when used:

               as an anchor

               as the first character of a bracket expression.

     $    The dollar sign is special when used as an anchor.

     Periods in EREs

          A period (.), when used outside a bracket expression, is an ERE that
          matches any character in the supported character set except NUL.

     EREs Matching Multiple Characters

          The following rules will be used to construct EREs matching multiple
          characters from EREs matching a single character:

     1.   A concatenation of EREs matches the concatenation of the character
          sequences matched by each component of the ERE. A concatenation of
          EREs enclosed in parentheses matches whatever the concatenation
          without the parentheses matches. For example, both the ERE cd and
          the ERE (cd) are matched by the third and fourth character of the
          string abcdefabcdef.

     2.   When an ERE matching a single character or an ERE enclosed in
          parentheses is followed by the special character plus-sign (+),
          together with that plus-sign it matches what one or more consecutive
          occurrences of the ERE would match. For example, the ERE b+(bc)
          matches the fourth to seventh characters in the string acabbbcde.
          And, [ab]+ and [ab][ab]* are equivalent.

     3.   When an ERE matching a single character or an ERE enclosed in
          parentheses is followed by the special character asterisk (*),
          together with that asterisk it matches what zero or more consecutive
          occurrences of the ERE would match.  For example, the ERE b*c
          matches the first character in the string cabbbcde, and the ERE b*cd
          matches the third to seventh characters in the string
          cabbbcdebbbbbbcdbc. And, [ab]* and [ab][ab] are equivalent when
          matching the string ab.

     4.   When an ERE matching a single character or an ERE enclosed in
          parentheses is followed by the special character question-mark (?),
          together with that question-mark it matches what zero or one
          consecutive occurrences of the ERE would match. For example, the ERE
          b?c matches the second character in the string acabbbcde.

     5.   When an ERE matching a single character or an ERE enclosed in
          parentheses is followed by an interval expression of the format {m},
          {m,} or {m,n}, together with that interval expression it matches
          what repeated consecutive occurrences of the ERE would match.  The
          values of m and  n will be decimal integers in the range 0 <= m <= n
          <= RE_DUP_MAX, where m specifies the exact or minimum number of
          occurrences and n specifies the maximum number of occurrences. The
          expression {m} matches exactly m occurrences of the preceding ERE,
          {m,} matches at least m occurrences and {m,n} matches any number of
          occurrences between m and n, inclusive.  For example, in the string
          abababccccccd the ERE c{3} is matched by characters seven to nine
          and the ERE (ab){2,} is matched by characters one to six.

     The behaviour of multiple adjacent duplication symbols (+, *, ? and
     intervals) produces undefined results.

     ERE Alternation

          Two EREs separated by the special character vertical-line (|) match
          a string that is matched by either. For example, the ERE a((bc)|d)
          matches the string abc and the string ad. Single characters, or
          expressions matching single characters, separated by the vertical
          bar and enclosed in parentheses, will be treated as an ERE matching
          a single character.

     ERE Precedence

          The order of precedence is as shown in the following table:

          BRE Precedence (from high to low)

               collation-related bracket symbols   [= =]  [: :]  [. .]

               escaped characters                  \<special character>

               bracket expression                  []

               grouping                            ()

               single-character-ERE duplication    *+?{m,n}

               concatenation

               anchoring                           ^ $

               alteration                          |

     For example, the ERE abba | cde matches either the string abba or the
     string cde (rather than the string abbade or abbcde, because
     concatenation has a higher order of precedence than alternation).

     ERE Expression Anchoring

          An ERE can be limited to matching strings that begin or end a line;
          this is called anchoring.  The circumflex and dollar sign special
          characters are considered ERE anchors when used anywhere outside a
          bracket expression. This has the following effects:

     1.   A circumflex (^) outside a bracket expression anchors the expression
          or subexpression it begins to the beginning of a string; such an
          expression or subexpression can match only a sequence starting at
          the first character of a string. For example, the EREs ^ab and (^ab)
          match ab in the string abcdef, but fail to match in the string
          cdefab, and the ERE a^b is valid, but can never match because the a
          prevents the expression ^b from matching starting at the first
          character.

     2.   A dollar sign ($) outside a bracket expression anchors the
          expression or subexpression it ends to the end of a string; such an
          expression or subexpression can match only a sequence ending at the
          last character of a string. For example, the EREs ef$ and (ef$)
          match ef in the string abcdef, but fail to match in the string
          cdefab, and the ERE e$f is valid, but can never match because the f
          prevents the expression e$ from matching ending at the last
          character.

     Regular Expression Grammar

          Grammars describing the syntax of both basic and extended regular
          expressions are presented in this section. The grammar takes
          precedence over the text.

     BRE/ERE Grammar Lexical Conventions

          The lexical conventions for regular expressions are as described in
          this section.

          Except as noted, the longest possible token or delimiter beginning
          at a given point will be recognised.

          The following tokens will be processed (in addition to those string
          constants shown in the grammar):

     COLL_ELEM      Any single-character collating element, unless it is a
                    META_CHAR.

     BACKREF        Applicable only to basic regular expressions. The
                    character string consisting of \ followed by a single-
                    digit numeral, 1 to 9.

     DUP_COUNT      Represents a numeric constant. It is an integer in the
                    range 0 <= DUP_COUNT <= RE_DUP_MAX. This token will only
                    be recognised when the context of the grammar requires it.
                    At all other times, digits not preceded by \ will be
                    treated as ORD_CHAR.

     META_CHAR      One of the characters:

                         ^   when found first in a bracket expression

                         -   when found anywhere but first (after an initial
                             ^, if any) or last in a bracket expression, or as
                             the ending
                             range point in a range expression

                         ]   when found anywhere but first (after an initial
                             ^, if any) in a bracket expression.

     L_ANCHOR       Applicable only to basic regular expressions. The
                    character ^ when it appears as the first character of a
                    basic regular expression and when not QUOTED_CHAR. The ^
                    may be recognised as an anchor elsewhere.

     ORD_CHAR       A character, other than one of the special characters in
                    SPEC_CHAR.

     QUOTED_CHAR    In a BRE, one of the character sequences:

                    \^      \.      \*      \[      \$      \\

                    In an ERE, one of the character sequences:

                    \^   \.   \[   \$   \(   \)  \|  \*   \+   \?   \{   \\

     R_ANCHOR       (Applicable only to basic regular expressions.) The
                    character $ when it appears as the last character of a
                    basic regular expression and when not QUOTED_CHAR. The $
                    may be recognised as an anchor elsewhere.

     SPEC_CHAR      For basic regular expressions, will be one of the
                    following special characters:

                    \  anywhere outside bracket expressions

                    [   anywhere outside bracket expressions

                    ^   when used as an anchor or when
                        first in a bracket expression

                    $   when used as an anchor

                    *   anywhere except: first in an entire RE;
                        anywhere in a bracket expression; directly
                        following \(; directly following an
                        anchoring ^.

                    For extended regular expressions, will be one of the
                    following special characters found anywhere outside
                    bracket expressions:

                    ^    .    [    $    (    )    |    *    +    ?    {    \

                    (The close-parenthesis is considered special in this
                    context only if matched with a preceding open-
                    parenthesis.)

RE and Bracket Expression Grammar
     This section presents the grammar for basic regular expressions,
     including the bracket expression grammar that is common to both BREs and
     EREs.

     %token ORD_CHAR QUOTED_CHAR DUP_COUNT
     %token BACKREF L_ANCHOR R_ANCHOR
     %token Back_open_paren  Back_close_paren
     /*    '\('              '\)'                     */
     %token Back_open_brace  Back_close_brace
     /*    '\{'              '\}'                     */
     /* The following tokens are for the Bracket Expression
        grammar common to both REs and EREs.              */
     %token COLL_ELEM META_CHAR
     %token Open_equal Equal_close Open_dot Dot_close Open_colon Colon_close
     /*    '[='         '=]'      '[.'    '.]'       '[:'        ':]'  */
     %token class_name
     /* class_name is a keyword to the LC_CTYPE locale category */
     /* (representing a character class) in the current locale  */
     /* and is only recognised between [: and :]                */
     %start basic_reg_exp
     %%
     /*             --------------------------------------------
                    Basic Regular Expression
                    --------------------------------------------
     */
     basic_reg_exp :          RE_expression
                   | L_ANCHOR
                   |                        R_ANCHOR
                   | L_ANCHOR               R_ANCHOR
                   | L_ANCHOR RE_expression
                   |          RE_expression R_ANCHOR
                   | L_ANCHOR RE_expression R_ANCHOR
                   ;

     RE_expression :               simple_RE
                   | RE_expression simple_RE
                   ;

     simple_RE     : nondupl_RE
                   | nondupl_RE RE_dupl_symbol
                   ;

     nondupl_RE    : one_character_RE
                   | Back_open_paren RE_expression Back_close_paren
                   | Back_open_paren Back_close_paren
                   | BACKREF
                   ;

     one_character_RE : ORD_CHAR
                      | QUOTED_CHAR
                      | '.'
                      | bracket_expression
                      ;

     RE_dupl_symbol : '*'
                    | Back_open_brace DUP_COUNT               Back_close_brace
                    | Back_open_brace DUP_COUNT ','           Back_close_brace
                    | Back_open_brace DUP_COUNT ',' DUP_COUNT Back_close_brace
                    ;

     /*                 --------------------------------------------
                        Bracket Expression
                        -------------------------------------------
     */
     bracket_expression : '[' matching_list    ']'
                        | '[' nonmatching_list ']'
                        ;

     matching_list      : bracket_list
                        ;

     nonmatching_list   : '^' bracket_list
                        ;

     bracket_list       : follow_list
                        | follow_list '-'
                        ;

     follow_list        :             expression_term
                        | follow_list expression_term
                        ;

     expression_term    : single_expression
                        | range_expression
                        ;

     single_expression  : end_range
                        | character_class
                        | equivalence_class
                        ;

     range_expression   : start_range end_range
                        | start_range '-'
                        ;

     start_range        : end_range '-'
                        ;

     end_range          : COLL_ELEM
                        | collating_symbol
                        ;

     collating_symbol   : Open_dot COLL_ELEM Dot_close
                        | Open_dot META_CHAR Dot_close
                        ;

     equivalence_class  : Open_equal COLL_ELEM Equal_close
                        ;

     character_class    : Open_colon class_name Colon_close
                        ;

     The BRE grammar does not permit L_ANCHOR or R_ANCHOR inside \( and \)
     (which implies that ^ and $ are ordinary characters).


ERE Grammar
     This section presents the grammar for extended regular expressions,
     excluding the bracket expression grammar.

     Note:  The bracket expression grammar and the associated %token
            lines are identical between BREs and EREs. It has been omitted
            from the ERE section to avoid unnecessary editorial duplication.

     %token ORD_CHAR QUOTED_CHAR DUP_COUNT
     %start extended_reg_exp
     %%
     /*               --------------------------------------------
                      Extended Regular Expression
                      --------------------------------------------
     */

     extended_reg_exp :                      ERE_branch
                      | extended_reg_exp ' | ' ERE_branch
                      ;

     ERE_branch       :            ERE_expression
                      | ERE_branch ERE_expression
                      ;

     ERE_expression   : one_character_ERE
                      | '^'
                      | '$'
                      | '(' extended_reg_exp ')'
                      | ERE_expression ERE_dupl_symbol
                      ;

     one_character_ERE : ORD_CHAR
                       | QUOTED_CHAR
                       | '.'
                       | bracket_expression
                       ;

     ERE_dupl_symbol  : '*'
                      | '+'
                      | '?'
                      | '{' DUP_COUNT               '}'
                      | '{' DUP_COUNT ','           '}'
                      | '{' DUP_COUNT ',' DUP_COUNT '}'
                      ;

     The ERE grammar does not permit several constructs that previous sections
     specify as having undefined results:

     o   ORD_CHAR preceded by \

     o   one or more ERE_dupl_symbols appearing first in an ERE,
         or immediately following |, ^ or (

     o   { not part of a valid ERE_dupl_symbol

     o   | appearing first or last in an ERE,
         or immediately following | or
         (, or immediately preceding ).

     Implementations are permitted to extend the language to allow these.
     Portable applications cannot use such constructs.

*/

#endif
