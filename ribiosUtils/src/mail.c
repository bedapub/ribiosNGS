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
/** @file mail.c
    @brief Send electronic mail.
    Module prefix mail_
*/
#include <unistd.h>
#include <stdarg.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/socket.h>
#include "log.h"
#include "format.h"

/// port of mail daemon
#define SMTPport 25
/// length of the read buffer
#define READSBUFLEN 2049

static struct sockaddr_in sin;
static  int clientSd = -1;

static Stringa gMsg = NULL;

static void allocateMsg (void) {
  stringCreateOnce (gMsg,200);
}

static char *encodeQuotedPrintable (char *s) {
  /*
    input: s
    returns: quoted-printable encoding
             (part of http://www.ietf.org/rfc/rfc1342.txt
             and http://www.ietf.org/rfc/rfc1652.txt)
    looking at the source code of http://www.fourmilab.ch/webtools/qprint/
    saved me some time
  */
  int needed = 0;
  static Stringa b = NULL;
  unsigned char *cp = (unsigned char*)(s - 1);
  unsigned char c;
  stringCreateClear(b,strlen (s+40));
  stringCpy (b,"=?ISO-8859-1?Q?");
  while ((c = *++cp) != '\0') {
    if (c >= 33 && c <= 126 && c != '=' && c != '?')
      stringCatChar (b,c);
    else {
      stringAppendf (b,"=%X",c & 0xFF);
      if (c != ' ' && c != '_' && c != '=' && c != '?')
        needed = 1;
    }
  }
  stringCat (b,"?=");
  if (!needed)
    stringCpy (b,s);
  return string (b);
}

static void getResponse (char *expectedStart) {
  char readsbuf[READSBUFLEN];
  int readslen;
  int len = (expectedStart != NULL) ? strlen (expectedStart) : 0;
  readslen = recv (clientSd,readsbuf,READSBUFLEN-1,0);
  readsbuf[readslen] = '\0';
  // printf ("read: (%d) '%s'\n",strlen (readsbuf),readsbuf);
  if (len != 0 && strncmp (readsbuf,expectedStart,len)) {
    allocateMsg ();
    stringPrintf(gMsg,
                 "expected response starting with '%s', but received '%s'",
                 expectedStart,readsbuf);
    warnAdd ("mail.getResponse",string (gMsg));
  }
}

static void sendText (char *text) {
  int len = strlen (text);
  int i = send (clientSd,text,len,0);
  if (i != len) {
    allocateMsg ();
    stringPrintf (gMsg,"count mismatch %d/%d, %s",i,len,strerror (errno));
    warnAdd ("mail.sendText",string (gMsg));
    // printf ("send: (%d) '%s'\n",strlen (text),text);
  }
}

static char *gMailUser = NULL;
static char *gMailPw = NULL;

void mail_setAuth (char *userB64,char *pwB64) {
  /**
     Sets user name and password of the sender
     @param[in] userB64 - user name in base 64
     @param[in] pwB64 - password in base 64
  */
  strReplace (&gMailUser,userB64);
  strReplace (&gMailPw,pwB64);
}

void mail_send (char *sender,char *recipients,char* subject,char *text)  {
  /**
     Send message 'text' to 'recipient' on behalf of 'sender'.<br>
     Precondition: if server requires AUTH, use mail_setAuth() before
     Exception handling: it is required to use warnCount() / warnReport()
       after calling this function to check for problems.
     @param[in] sender - should be the real and valid eMail address of
                         the sender.
                         To add a sender description to be displayed, use
                         the format "senderEmail senderDescription",
                         e.g. "abc@company.com ABC".
     @param[in] recipients - eMail adr(s) to send to. Recipients are seperated
                             by blanks, commas, semicolons or tabs.
     @param[in] subject - subject text line or NULL
     @param[in] text - text to send, null-terminated; may not contain
                       the substring '\n.\n' (!), since this indicates
                       end of message
  */
  struct hostent *hp;
  char *hostname = "localhost";
  char buf[2048];
  WordIter wi;
  char *word;
  char *cp = NULL;
  int i;
  static Stringa textStr = NULL;
  static char *senderBuf = NULL;
  static char *recpBuf = NULL;
  stringCreateClear (textStr,200);
  strReplace (&senderBuf,sender);
  strReplace (&recpBuf,recipients);

  if (strstr (text,"\n.\n")) {
    allocateMsg ();
    stringPrintf (gMsg,
                  "text contains mail terminator; cannot be sent: '%s'",
                  text);
    warnAdd ("mail_send",string (gMsg));
  }

  if ((hp = gethostbyname (hostname)) == 0) {
    allocateMsg ();
    stringPrintf (gMsg,"gethostbyname %s",strerror (errno));
    warnAdd ("mail_send",string (gMsg));
    return;
  }

  memset (&sin,0,sizeof (sin));
  sin.sin_family = AF_INET;
  sin.sin_addr.s_addr = ((struct in_addr *) (hp->h_addr))->s_addr;
  sin.sin_port = htons (SMTPport);
  if ((clientSd = socket (AF_INET,SOCK_STREAM,0)) == -1) {
    allocateMsg ();
    stringPrintf (gMsg,"socket %s",strerror (errno));
    warnAdd ("mail_send",string (gMsg));
    return;
  }

  if (connect (clientSd,&sin,sizeof (sin)) == -1) {
    allocateMsg ();
    stringPrintf (gMsg,"connect %s",strerror (errno));
    warnAdd ("mail_send",string (gMsg));
    return;
  }

  getResponse ("220");
  sendText ("HELO localhost\n");
  if (warnCount (NULL,NULL) != 0)
    return;
  getResponse ("250 ");
  if (warnCount (NULL,NULL) != 0)
    return;
  if (gMailUser != NULL) {
    sendText ("AUTH LOGIN\n");
    if (warnCount (NULL,NULL) != 0)
      return;
    getResponse ("334 VXNlcm5hbWU6"); // ???
    if (warnCount (NULL,NULL) != 0)
      return;
    sprintf (buf,"%s\n",gMailUser);
    sendText (buf);
    if (warnCount (NULL,NULL) != 0)
      return;
    getResponse ("334 UGFzc3dvcmQ6");
    if (warnCount (NULL,NULL) != 0)
      return;
    sprintf (buf,"%s\n",gMailPw);
    sendText (buf);
    if (warnCount (NULL,NULL) != 0)
      return;
    getResponse ("235 Authentication succeeded");
    if (warnCount (NULL,NULL) != 0)
      return;
  }
  if ((cp = strchr (senderBuf,' ')) != NULL) {
    *cp = '\0';
    stringAppendf (textStr,"From: %s (%s)\n",senderBuf,cp+1);
  }
  sprintf (buf,"MAIL FROM: %s\n",senderBuf);
  sendText (buf);
  if (warnCount (NULL,NULL) != 0)
    return;
  getResponse ("250 ");
  if (warnCount (NULL,NULL) != 0)
    return;
  if (cp)
    *cp = ' ';
  stringAppendf (textStr,"To: ");
  i = -1;
  wi = wordIterCreate (recpBuf," ,;\t\n",1);
  while ((word = wordNext (wi)) != NULL) {
    sprintf (buf,"RCPT TO: %s\n",word);
    sendText (buf);
    if (warnCount (NULL,NULL) != 0)
      return;
    getResponse ("250 ");
    if (warnCount (NULL,NULL) != 0)
      return;
    if (++i)
      stringCatChar (textStr,',');
    stringCat (textStr,word);
  }
  stringCatChar (textStr,'\n');
  sendText ("DATA\n");
  if (warnCount (NULL,NULL) != 0)
    return;
  getResponse ("354 ");
  if (warnCount (NULL,NULL) != 0)
    return;
  if (subject != NULL && !isBlankStr (subject))
    stringAppendf (textStr,"Subject: %s\n",encodeQuotedPrintable (subject));
  stringCat (textStr,text);
  sendText (string (textStr));
  if (warnCount (NULL,NULL) != 0)
    return;
  sendText ("\n.\n");
  if (warnCount (NULL,NULL) != 0)
    return;
  getResponse ("250 ");
  if (warnCount (NULL,NULL) != 0)
    return;
  if (shutdown (clientSd,SHUT_RDWR) == -1) {
    allocateMsg ();
    stringPrintf (gMsg,"shutdown %s",strerror (errno));
    warnAdd ("mail_send",string (gMsg));
    return;
  }
  if (close (clientSd) == -1) {
    allocateMsg ();
    stringPrintf (gMsg,"close %s",strerror (errno));
    warnAdd ("mail_send",string (gMsg));
  }
}

void mail_send_subject (char *recipients,char *subject,char *text)  {
  /**
     What is the difference between mail_send() and mail_send_subject()?
     mail_send() talks SMTP with the local mail server while
     mail_send_subject() uses the command line Mail tool.
     Usually both should work, but I am not 100% sure about mail_send().
     Execption handling: it is required to use warnCount() / warnReport()
       after calling this function to check for problems.
     @param[in] recipients - eMail adr(s) to send to. Recipients are seperated
                             by blanks, commas, semicolons or tabs.
     @param[in] subject - subject text line
     @param[in] text - text to send, null-terminated; may not contain
                       the substring '\n.\n' (!), since this indicates
                       end of message
  */
  int exitStatus;
  FILE *p;
  Stringa s = stringCreate (100);
  stringPrintf (s,"/usr/bin/Mail -s '%s' %s",subject,recipients);
  p = popen (string (s),"w");
  if (p == NULL) {
    allocateMsg ();
    stringPrintf (gMsg,"popen: %s: %s",string (s),strerror (errno));
    warnAdd ("mail_send_subject", string (gMsg));
    return;
  }
  fputs (text,p);
  if ((exitStatus = pclose (p)) != 0) {
    allocateMsg ();
    stringPrintf (gMsg,"pclose: exitStatus=%d: %s",
                  exitStatus,strerror (errno));
    warnAdd ("mail_send_subject",string (gMsg));
  }
  stringDestroy (s);
}
