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
/** @file notifier.c
    @brief Client for the event notification service:
    "Automatic Event Notifier".
    Module prefix notifier_
*/
#include "log.h"
#include "hlrmisc.h"
#include "http.h"
#include "linestream.h"
#include "notifierconf.h"
#include "notifier.h"

static int gUseDevelopment = 0;

void notifier_useDevelopment (void) {
  /**
     if called, notifier_addEvent() adds an event to the
     notifier development version
  */
  gUseDevelopment = 1;
}

char *notifier_addEvent (char *application,Texta domains,char *message) {
  /**
     Posts an event to the notifer system.<br>
     Postcondition: users registered for receiving notification
                    concerning 'application' and 'domain' will receive an eMail
     @param[in] application - name of application generating the event,
                              e.g. "ranno"
     @param[in] domains - event types (must exist in the corresponding
                          domain_table).
                          Applications can differenatiate between event types
                          if they wish.
                          NULL if such a differentiation is not needed.
                          if there are no domains, else if every registered user
                          of any domain and the corresponding application
                          should receive the message.
     @param[in] message - the text to be displayed in the user notification.
     @return NULL if no error has occurred, else the error message is returned.
             Memory returned is managed by this function.
             Return value is stable until next function call.
  */
  char *items[] = { "application","domain","message" };
  char *values[NUMELE (items)];
  char *line;
  char *status = NULL;
  char *stderror;
  int i;
  Stringa domain = stringCreate (50);
  static char* ret_message = NULL;
  LineStream ls;

  if (domains != NULL) {
    for (i=0;i<arrayMax (domains);i++) {
      stringCatChar (domain,',');
      stringCat (domain,textItem (domains,i));
    }
    stringCatChar (domain,',');
  }
  values[0] = application;
  values[1] = (char *)(domains ? string (domain) : ",NULL,");
  values[2] = message;

  if (gUseDevelopment)
    cgiPost (NOTIFIER_EVENT_ADD_URL_DEV,NUMELE (items),items,values);
  else
    cgiPost (NOTIFIER_EVENT_ADD_URL,NUMELE (items),items,values);
  ls = ls_createFromBuffer (cgiRecv ());
  // get xml result: check status and extract error message
  while ((line = ls_nextLine (ls)) != NULL) {
    if ((status = strstr (line,"<STATUS>")) != NULL)
      break;
  }
  if (status == NULL)
    strReplace (&ret_message,
                "PROBLEM: notifier_addEvent: no xml-status line received");
  else {
    hlr_free (ret_message);
    if (status[8] != '0') { // not OK
      while ((line = ls_nextLine (ls)) != NULL) {
        if ((stderror = strstr (line,"<STDERR>")) != NULL) {
          strReplace (&ret_message,stderror+8);
          if ((stderror = strstr (ret_message,"</STDERR>")) != NULL)
            *stderror = '\0';
        }
      }
    }
  }
  ls_destroy (ls);
  stringDestroy (domain);
  return ret_message;
}

char *notifier_deleteEvent (char *application,Texta domains,char *message) {
  /**
     Deletes an event from the notifer system
     @param[in] application - name of application deleting the event,
                              e.g. "ranno"
     @param[in] domains - event types (must exist in the corresponding
                          domain_table).
                          NULL if such a differentiation was not used
                          for creating the event.
     @param[in] message - the text of the user notification.
     @return NULL if no error has occurred, else the error message is returned.
             Memory returned is managed by this function.
             Return value is stable until next function call.
  */
  char *items[] = { "application","domain","message" };
  char *values[NUMELE (items)];
  char *line;
  char *status = NULL;
  char *stderror;
  int i;
  Stringa domain = stringCreate (50);
  static char* ret_message = NULL;
  LineStream ls;

  if (domains != NULL) {
    for (i=0;i<arrayMax (domains);i++) {
      stringCatChar (domain,',');
      stringCat (domain,textItem (domains,i));
    }
    stringCatChar (domain,',');
  }
  values[0] = application;
  values[1] = (char *)(domains ? string(domain) : ",NULL,");
  values[2] = message;
  if (gUseDevelopment)
    cgiPost (NOTIFIER_EVENT_DELETE_URL_DEV,NUMELE (items),items,values);
  else
    cgiPost (NOTIFIER_EVENT_DELETE_URL,NUMELE (items),items,values);
  ls = ls_createFromBuffer (cgiRecv ());
  // get xml result: check status and extract error message
  while ((line = ls_nextLine (ls)) != NULL) {
    if ((status = strstr (line,"<STATUS>")) != NULL)
      break;
  }
  if (status == NULL)
    strReplace (&ret_message,
                "PROBLEM: notifier_deleteEvent: no xml-status line received");
  else {
    hlr_free (ret_message);
    if (status[8] != '0') { // not OK
      while ((line = ls_nextLine (ls)) != NULL) {
        if ((stderror = strstr (line,"<STDERR>")) != NULL) {
          strReplace (&ret_message,stderror+8);
          if ((stderror = strstr (ret_message,"</STDERR>")) != NULL)
            *stderror='\0';
        }
      }
    }
  }
  ls_destroy (ls);
  stringDestroy (domain);
  return ret_message;
}
