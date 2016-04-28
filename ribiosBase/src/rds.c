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
/** @file rds.c
    @brief Provides access to user information from usrman.
    Module prefix rds_
*/
#include "log.h"
#include "format.h"
#include "hlrmisc.h"
#include "html.h"
#include "http.h"
#include "linestream.h"
#include "biosdefs.h"
#include "rds.h"

/// cgi to look up information about a user (name, email, etc)
#define RDS_USERINFOCGI "http://bioinfoc.ch/bicgi/userinfocgi"

/// parameter: userid
#define RDS_USERID_URL RDS_USERINFOCGI "?-plain+%s"

char *rds_get (char *userid,int column) {
  /**
     Retrieve specified column from usrman table info for userid.<br>
     @param[in] userid - user id, must not be NULL or empty. search is
                         case-sensitive.
     @param[in] column - information to retrieve for user identified
                         by userid, valid values are defined in rds.h, if you're
                         not sure about it use the macros above.
     @return pointer to requested string, may be NULL if userid not found or
             if multiple hits found, may be empty if no info in rds. the memory
             pointed to is read-only for the user of this function.
             value is valid until next successfull call to this function.
  */
  static Stringa url = NULL;
  static char *retval = NULL;
  char *buffer;
  LineStream ls;
  char *line;
  char *cp;
  char *name;
  char *value;

  if ((column < 0) || (column > RDS_LAST))
    die ("rds_get: invalid column number %d",column);
  if (userid == NULL || *userid == '\0')
    die ("rds_get: invalid userid");
  stringCreateOnce (url,50);
  stringPrintf (url,RDS_USERID_URL,userid);
  cgiGet (string (url));
  buffer = cgiRecv ();
  if (buffer == NULL)
    die ("rds_get: failed for: %s",url);
  ls = ls_createFromBuffer (buffer);
  while ((line = ls_nextLine (ls)) != NULL) {
    if (!(cp = strchr (line,'\t'))) {
      warn ("rds_get: line ignored - cannot find \\t in line:\n%s\n",
            line);
      continue;
    }
    *cp = '\0';
    name = line;
    value = cp+1;
    if (column == RDS_USERID && strEqual (name,"username")) {
      strReplace (&retval,value);
      break;
    }
    else if (column == RDS_FIRSTNAME && strEqual (name,"firstname")) {
      strReplace (&retval,value);
      break;
    }
    else if (column == RDS_LASTNAME && strEqual (name,"lastname")) {
      strReplace (&retval,value);
      break;
    }
    else if (column == RDS_COMPANY && strEqual (name,"company")) {
      strReplace (&retval,value);
      break;
    }
    else if (column == RDS_EMAIL && strEqual (name,"email")) {
      strReplace (&retval,value);
      break;
    }
    else if (column == RDS_CREATED && strEqual (name,"created")) {
      strReplace (&retval,value);
      break;
    }
    else if (column == RDS_LASTUPDATED && strEqual (name,"lastupd")) {
      strReplace (&retval,value);
      break;
    }
  }
  if (line == NULL)
    die ("rds_get: nothing found for field %d",column);
  ls_destroy (ls);
  return retval;
}

char *rds_getName (char *userid) {
  /**
     @param[in] userid - e.g. doej
     @return first and last name, e.g. "John Doe"
             or "" if userid does not exist or other problem.
             Memory managed by this function; read/write by user ok,
             but free or realloc forbidden; stable until next call
             to this function
  */
  static Stringa s = NULL;
  char *firstName = rds_getFirstName (userid);
  stringCreateClear (s,50);
  if (firstName != NULL) {
    stringCpy (s,firstName);
    stringCatChar (s,' ');
    stringCat (s,rds_getLastName (userid));
  }
  return string (s);
}
