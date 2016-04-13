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
/** @file identificator.c
    @brief Knows how to verify the identity of a user.
    Module prefix ident_
*/
#include <pwd.h>
#include <crypt.h>
#include "log.h"
#include "format.h"
#include "rofutil.h"
#include "identificatorconf.h"

#if IDENTIFICATOR_USE_RDS == 1
#include "rds.h"
#endif

#if IDENTIFICATOR_USE_PAM == 1
#include <security/pam_appl.h>
#endif

#include "identificator.h"

// ---------------------- private functions  --------------------------

// ------------------------ okUnix ------------------------------------
#if IDENTIFICATOR_USE_UNIX == 1
static int okUnix (char *user,char *password) {
  /**
     Check if a username / password combination is valid on the UNIX
     machine that this process is running on.<br>
     Restriction: this only works as long as password shadowing is not
                  used. if it is used, this program must run setuid root.
     @param[in] user - username
     @param[in] password -
     Rreturn 1 if password correct for username, else 0
  */
  char salt[2];
  struct passwd *pw;
  char *pwd;

  if ((pw = getpwnam (user)) == NULL)
    return 0;
  strncpy (salt,pw->pw_passwd,2);
  pwd = crypt (password,salt);
  if (strEqual (pwd,pw->pw_passwd))
    return 1;
  return 0;
}
#endif

// ------------------------ okPam -----------------------------------
#if IDENTIFICATOR_USE_PAM == 1
static int okPam (char *userid,char *passwd) {
  /**
     Checks user/passwd combination against PAM (pluggable authentication
     module architecture).<br>
     Note: depends on configuration of PAM in /etc/pam.conf (see 'man pam.conf')
           is used in morphochem with pam_smb_auth.so.1 for sharing NT passwds
           when using pam_unix.so.1 there's the same problem as with okUnix():
           the process needs to run as root to be able to read shadowed
           passwords...
     @aram[in] userid - the userid to be authenticated
     @param[in] passwd - the passwd to be checked
     @return 1 if check ok, 0 if some failure
  */
  int result = 0;
  const char *service = "iwbi"; // service name (for lookup in /etc/pam.conf ?)
  const char *user = userid; // user id to be authenticated
  struct pam_conv pam_conv; // conversation structure for PAM callback
  pam_handle_t *pamh = NULL; // PAM handle for subsequent reference to transaction
  int auth = 0;
  pam_conv.conv = NULL; // we don't need no conversation... :-)
  pam_conv.appdata_ptr = NULL; // ... and no application-specific data either
  pam_start (service,user,&pam_conv,&pamh);
  pam_set_item (pamh,PAM_AUTHTOK,passwd);
  auth = pam_authenticate (pamh,PAM_SILENT);
  if (auth == PAM_SUCCESS)
    result = 1;
  /*
    good for debugging, not needed for normal use
    these are the return values documented in the man pages only;
    but some others are frequently occurring, they can be looked up in
    /usr/include/security/pam_appl.h

    switch (auth) {
      case (PAM_SUCCESS):
        result = 1;
        puts ("\tsuccess.");
        break;
      case (PAM_AUTH_ERR):
        puts ("\tauthentication failure.");
        break;
      case (PAM_CRED_INSUFFICIENT):
        puts ("\tinsufficient credentials.");
        break;
      case (PAM_AUTHINFO_UNAVAIL):
        puts ("\tauthentication information unavailable.");
        break;
      case (PAM_USER_UNKNOWN):
        puts ("\tuser unknown.");
        break;
      case (PAM_MAXTRIES):
        puts ("\tmaximum tries exceeded.");
        break;
      default:
        printf ("undefined return code %d from pam_authenticate().\n",auth);
        break;
    }
  */
  pam_end (pamh,/*status*/0);
  return result;
}
#endif

// ------------------------ okIndependent --------------------------------
#if IDENTIFICATOR_USE_INDEPENDENT == 1
static int okIndependent (char *user,char *password) {
  /**
     Check if a username / password combination is valid the
     database for external accounts.<br>
     If INDEPACCOUNT_CHECK is NULL, okIndependent always returns 0
     @param[in] user - username
     @param[in] password
     @return 1 if password correct for username, else 0
  */
  int status;
  FILE *p;
  if (INDEPACCOUNT_CHECK == NULL)
    return 0;
  p = popen (INDEPACCOUNT_CHECK,"w");
  fprintf (p,"%s %s\n",user,password);
  status = pclose (p);
  if (status == 0x100)
    return 1;
  return 0;
}
#endif

// ------------------------ okUsrman --------------------------------
#if IDENTIFICATOR_USE_USRMAN == 1
static int okUsrman (char *user,char *password) {
  /**
     Check if a username / password combination is valid in usrman.<br>
     If USRMAN_CHECK is NULL, okUsrman always returns 0
     @param[in] user - username
     @param[in] password
     @return 1 if password correct for username, else 0
  */
  int status;
  FILE *p;
  if (USRMAN_CHECK == NULL)
    return 0;
  p = popen (USRMAN_CHECK,"w");
  fprintf (p,"%s %s\n",user,password);
  status = pclose (p);
  if (status == 0x100)
    return 1;
  return 0;
}
#endif

// ---------------------- public functions  --------------------------

static FILE *gLogfile = NULL;

void ident_init (void) {
  /**
     Initialize identification service for being used.<br>
     Precondition: none<br>
     Postcondition: ident_ok() can be called<br>
                    ident_end() can be called
  */
  if (gLogfile != NULL)
    die ("ident_init() twice");
  gLogfile = hlr_fopenAppend (IDENTIFICATOR_LOG);
}

int ident_ok (char *user,char *password) {
  /**
     Checks if a username / password combination is valid.<br>
     Precondition: ident_init() was called
     @param[in] user - user name
     @param[in] password -
     @return 1 if password correct for username, else 0
  */
  char *cp;
  if (gLogfile == NULL)
    die ("ident_ok() without successful ident_init()");
  if (*user == '\0')
    return 0;
  // the order of checks is by descending speed of the service
#if IDENTIFICATOR_USE_UNIX == 1
  // unix password
  if (okUnix (user,password)) {
    logPrintTime (gLogfile);
    fprintf (gLogfile,"%s authorized via bioinfo unix.\n",user);
    return 1;
  }
#endif

#if IDENTIFICATOR_USE_INDEPACCOUNT == 1
  // independent account password database for external users
  if (okIndependent (user,password)) {
    logPrintTime (gLogfile);
    fprintf (gLogfile,"%s authorized via independent account db.\n",user);
    return 1;
  }
#endif

#if IDENTIFICATOR_USE_USRMAN == 1
  // usrman database for external users
  if (okUsrman (user,password)) {
    logPrintTime (gLogfile);
    fprintf (gLogfile,"%s authorized via usrman db.\n",user);
    return 1;
  }
#endif

#if IDENTIFICATOR_USE_RDS == 1
  // Roche Directory service / LDAP protocol
  if (rds_userpwok (user,password)) {
    logPrintTime (gLogfile);
    fprintf (gLogfile,"%s authorized via AD.\n",user);
    return 1;
  }
#endif

#if IDENTIFICATOR_USE_PAM == 1
  // authentication via PAM
  if (okPam (user,password)) {
    logPrintTime (gLogfile);
    fprintf (gLogfile,"%s authorized via PAM.\n",user);
    return 1;
  }
#endif

  // user / password combination is not valid
  logPrintTime (gLogfile);
  fprintf (gLogfile,"authorization for user '%s'",user);
  if ((cp = getenv ("REMOTE_HOST")) != NULL)
    fprintf (gLogfile," coming from %s",cp);
  fprintf (gLogfile," failed.\n");
  return 0;
}

void ident_end (void) {
  /**
     Stop using identification service.
  */
  fclose (gLogfile);
  gLogfile = NULL;
}
