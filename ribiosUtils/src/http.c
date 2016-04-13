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
/** @file http.c
    @brief HTTP GET and POST stuff:
    core communication routines of a WWW browser.
    Module prefix cgi_
*/
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include "plabla.h"
#include "log.h"
#include "format.h"
#include "html.h"
#include "rofutil.h"
#include "linestream.h"
#include "pwdecode.h"
#include "http.h"
#include "biosdefs.h"

static int gSd = -1;

static int httpOpen (char *host,int port) {
  /**
     Opens an http connection.<br>
     Postcondition: warnCount() may be increased.
     @param[in] host - e.g. bioinfoc.ch
     @param[in] port - a port number, 0 means default http port 80
     @return 0 if ok, else a number > 0
  */
  struct sockaddr_in pin;
  struct hostent *hp;

  if ((hp = gethostbyname (host)) == NULL) {
    warnAdd ("httpOpen","Error in gethostbyname");
    return 1;
  }
  memset (&pin,'\0',sizeof (pin));
  pin.sin_family = AF_INET;
  pin.sin_addr.s_addr = ((struct in_addr *)(hp->h_addr))->s_addr;
  if (port == 0)
    port = 80; // default for WWW
  pin.sin_port = htons (port);
  if ((gSd = socket (AF_INET,SOCK_STREAM,0)) == -1) {
    warnAdd ("httpOpen","Error in socket");
    return 2;
  }
  if (connect (gSd,&pin,sizeof (pin)) == -1) {
    warnAdd ("httpOpen","Error in connect");
    return 3;
  }
  return 0;
}

static void httpClose (void) {
  /**
     Close the http connection.
  */
  close (gSd);
  gSd = -1;
}

/* ------------- mini module: URL parsing ----------------- */
/// the user part of an URL
#define URLUSER 0
/// the host part of an URL
#define URLHOST 1
/// the port part of an URL
#define URLPORT 2
/// the program part of an URL
#define URLPROG 3

static char *urlUser = NULL;
static char *urlHost = NULL;
static char *urlPort = NULL;
static char *urlProg = NULL;

static void urlParse (char *url) {
  /**
     Parses a url into username:password, host, port and program
     (/ and what follows after host and port).<br>
     Postcondition: urlGetPart() works
     @param[in] a complete url
  */
  char *pos1,*pos2;
  static char *urlw = NULL;
  int offs;

  hlr_free (urlUser);
  hlr_free (urlHost);
  hlr_free (urlPort);
  hlr_free (urlProg);
  strReplace (&urlw,url);

  if (!strStartsWithC (urlw,"http://") && !strStartsWithC (urlw,"https://"))
    die ("urlParse: URL does not start with http:// or https://: %s",url);
  if (strStartsWithC (urlw,"http://"))
    offs = 7;
  else
    offs = 8;
  pos1 = strchr (urlw+offs,'/');
  if (!pos1)
    die ("urlParse: URL does not have a path: %s",url);
  *pos1 = '\0';
  pos2 = strchr (urlw+7,'@');
  if (pos2) {
    *pos2 = '\0';
    urlUser = hlr_strdup (urlw+offs);
    urlHost = hlr_strdup (pos2+1);
  }
  else
    urlHost = hlr_strdup (urlw+offs);
  pos2 = strchr (urlHost,':');
  if (pos2) {
    *pos2 = '\0';
    urlPort = hlr_strdup (pos2+1);
  }
  *pos1 = '/'; // put / back
  urlProg = hlr_strdup (pos1);
}

static char *urlGetPart (int which) {
  /**
     Returns a partial url (either host or port or program or user);
     assumes that urlParse has been called, otherwise returns NULL
     @param[in] which - part of UTL to be returned
     @return the requested part
  */
  if (which == URLPORT)
    return urlPort;
  else if (which == URLHOST)
    return urlHost;
  else if (which == URLUSER)
    return urlUser;
  else if (which == URLPROG)
    return urlProg;
  else
    return NULL;
}

/* ------------- end mini module: URL parsing ----------------- */

static int gProxyCopyConfigFromHFile = 1;
static char *gProxy = NULL;
static char *gProxyException = NULL;

void cgiProxySet (char *proxy,char *exception) {
  /**
     Set a proxy.<br>
     Usage: proxy defaults are defined at compile time in biosdefs.h;
            these defaults can be overriden using this function
     @param[in] proxy - URL of http proxy server, e.g.
                        "http://www-proxy1.xxx.com:8080/";
                        must no be NULL; use "" to disable proxy.
     @param[in] exception - tail of host names that do not need the proxy, e.g.
                            ".xxx.com" - must be lower case;<br>
                            use "" do say 'no exceptions';
                            must not be NULL if proxy given
  */
  if (proxy == NULL)
    die ("cgiProxySet (NULL,)");
  strReplace (&gProxy,proxy);
  if (*gProxy != '\0') {
    if (exception == NULL)
      die ("cgiProxySet (,NULL)");
    strReplace (&gProxyException,exception);
  }
  else
    strReplace (&gProxyException,"");
  gProxyCopyConfigFromHFile = 0;
}

/// The cookies to send
Stringa gSendCookies = NULL;

void cgiSendCookieReset (void) {
  /**
     Clear list of cookie(s) to send by cgiGet()<br>
     Precondition: none<br>
     Postcondition: cgiGet() will not send any cookie
  */
  stringDestroy (gSendCookies);
}

void cgiSendCookieAdd (char *name,char *value) {
  /**
     Add cookie to next cgiGet() call.<br>
     Precondition: none<br>
     Postcondition: cgiGet()/cgiPost() will send cookie 'name'='value'
     @param[in] name - name of cookie
     @param[in] value - value of cookie; must not be NULL
  */
  static Stringa e = NULL;

  stringCreateOnce (gSendCookies,100);
  hlr_assert (value,"cgiSendCookieAdd() needs value");
  stringCreateOnce (e,40);
  cgiEncodeWord (value,e);
  stringAppendf (gSendCookies,"Cookie: %s=%s\r\n",name,string (e));
}

int cgiSendCookiePresent (char *name) {
  /**
     Check whether a certain cookie is set
     @param[in] name - cookie name to search for
     @return 1 if cookie with 'name' has been set using cgiSendCookieAdd()
             else 0
  */
  static Stringa s = NULL;
  if (gSendCookies == NULL)
    return 0;
  stringCreateOnce (s,strlen (name)+40);
  stringPrintf (s,"Cookie: %s=",name);
  return strstr (string (s),name) ? 1 : 0;
}

/* ---------------------------- base64 stuff ------------ */
/* the following functions are suitable only for short strings
   (~<70 char) e.g. username:password
*/

static Stringa base64out = NULL;
static char index_64[128] = {
  -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
  -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,-1,
  -1,-1,-1,-1, -1,-1,-1,-1, -1,-1,-1,62, -1,-1,-1,63,
  52,53,54,55, 56,57,58,59, 60,61,-1,-1, -1,-1,-1,-1,
  -1, 0, 1, 2,  3, 4, 5, 6,  7, 8, 9,10, 11,12,13,14,
  15,16,17,18, 19,20,21,22, 23,24,25,-1, -1,-1,-1,-1,
  -1,26,27,28, 29,30,31,32, 33,34,35,36, 37,38,39,40,
  41,42,43,44, 45,46,47,48, 49,50,51,-1, -1,-1,-1,-1
};

/// base 64 encode a charcter
#define char64(c) (((unsigned)(c) > 127) ? -1 : index_64[(unsigned)(c)])

static void output64chunk (char c1,char c2,char c3,int pads) {
  char oneChar[] = " ";
  char basis_64[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

  oneChar[0] = basis_64[c1>>2];
  stringCat (base64out,oneChar);
  oneChar[0] = basis_64[((c1 & 0x3)<< 4) | ((c2 & 0xF0) >> 4)];
  stringCat (base64out,oneChar);
  if (pads == 2) {
    oneChar[0] = '=';
    stringCat (base64out,oneChar);
    oneChar[0] = '=';
    stringCat (base64out,oneChar);
  }
  else if (pads) {
    oneChar[0] = basis_64[((c2 & 0xF) << 2) | ((c3 & 0xC0) >>6)];
    stringCat (base64out,oneChar);
    oneChar[0] = '=';
    stringCat (base64out,oneChar);
  }
  else {
    oneChar[0] = basis_64[((c2 & 0xF) << 2) | ((c3 & 0xC0) >>6)];
    stringCat (base64out,oneChar);
    oneChar[0] = basis_64[c3 & 0x3F];
    stringCat (base64out,oneChar);
  }
}

char *toBase64 (char *in) {
  /**
     Encode a short string in base64, only one line
     @param[in] in - a C string
     @return encoded string, its memory belongs to the module and is valid
             until another call to toBase64 or fromBase64 is made
  */
  int c1,c2,c3;
  int i,l;

  stringCreateClear (base64out,20);
  l = strlen (in);
  i = 0;
  while (i < l && (c1 = in[i++])) {
    if (i < l)
      c2 = in[i++];
    else
      c2 = '\0';
    if (!c2)
      output64chunk (c1,0,0,2);
    else {
      if (i < l)
        c3 = in[i++];
      else
        c3 = '\0';
      if (!c3)
        output64chunk (c1,c2,0,1);
      else
        output64chunk (c1,c2,c3,0);
    }
  }
  return string (base64out);
}

char* fromBase64 (char *in) {
  /**
     Decode a short string from base64
     @param[in] in - a base64 encoded C string, only one line
     @return decoded string, its memory belongs to the module and is valid
             until another call to toBase64 or fromBase64 is made
  */
  char c1,c2,c3,c4;
  int dataDone = 0;
  int i,l;
  char oneChar[] = " ";

  stringCreateClear (base64out,20);
  l = strlen (in);
  i = 0;
  while (i < l && (c1 = in[i++])) {
    if (dataDone)
      continue;
    if (i < l)
      c2 = in[i++];
    else
      c2 = '\0';
    if (i < l)
      c3 = in[i++];
    else
      c3 = '\0';
    if (i < l)
      c4 = in[i++];
    else
      c4 = '\0';
    if (c2 == '\0' || c3 == '\0' || c4 == '\0')
      die ("fromBase64: decoder saw premature end of string!");
    if (c1 == '=' || c2 == '=') {
      dataDone = 1;
      continue;
    }
    c1 = char64 (c1);
    c2 = char64 (c2);
    oneChar[0] = ((c1<<2) | ((c2&0x30)>>4));
    stringCat (base64out,oneChar);
    if (c3 == '=')
      dataDone = 1;
    else {
      c3 = char64 (c3);
      oneChar[0] = (((c2&0XF) << 4) | ((c3&0x3C) >> 2));
      stringCat (base64out,oneChar);
      if (c4 == '=')
        dataDone = 1;
      else {
        c4 = char64 (c4);
        oneChar[0] = (((c3&0x03) <<6) | c4);
        stringCat (base64out,oneChar);
      }
    }
  }
  return string (base64out);
}

/* ------------------------------------------------ */

int cgiGet (char *url) {
  /**
     Prepare fetching the document addressed by 'URL'
     using the HTTP GET method<br>
     Precondition: if http proxy requried it must have been defined at
                   compile time in biosdefs.h or at run time using
                   cgiProxySet(), optional: cgiSendCookieAdd()<br>
     Postcondition: cgiRecv() can be called;
                    warnCount()/warnReport() may be increased/show messsage
     @param[in] url = the URL to get from
     @return 0 if ok, else non-zero
  */
  int ret;
  int port;
  static Stringa inLine = NULL;
  static char *s = NULL;
  char *p = NULL;

  if (gProxyCopyConfigFromHFile == 1)
    cgiProxySet (HTTP_PROXY,HTTP_PROXY_EXCEPTION);
  urlParse (url);
  strReplace (&s,urlGetPart (URLHOST));
  tolowerStr (s);
  if (*gProxy != '\0' &&
      (*gProxyException == '\0' || !strEndsWith (s,gProxyException))) {
    urlParse (gProxy);
    p = url;
  }
  else
    p = urlGetPart (URLPROG);
  if (!urlGetPart (URLPORT))
    port = 0;
  else
    port = atoi (urlGetPart (URLPORT));
  stringCreateOnce (inLine,100);
  if ((ret = httpOpen (urlGetPart (URLHOST),port)) != 0) {
    stringPrintf (inLine,"Could not open TCPIP connection %s:%d",
                  urlGetPart (URLHOST),port);
    warnAdd ("cgiGet",string (inLine));
    return ret;
  }
  stringPrintf (inLine,"GET %s HTTP/1.0\r\n",p);
  write (gSd,string (inLine),stringLen (inLine));
  if (urlGetPart (URLUSER)) {
    stringPrintf (inLine,"Authorization: basic %s\r\n",
                  toBase64 (urlGetPart (URLUSER)));
    write (gSd,string (inLine),stringLen (inLine));
  }
  stringCpy (inLine,"User-Agent: http.c\r\n");
  write (gSd,string (inLine),stringLen (inLine));
  stringCpy (inLine,"Accept: */*\r\n");
  write (gSd,string (inLine),stringLen (inLine));
  stringPrintf (inLine,"Host: %s\r\n",urlGetPart (URLHOST));
  write (gSd,string (inLine),stringLen (inLine));
  if (gSendCookies != NULL)
    write (gSd,string (gSendCookies),stringLen (gSendCookies));
  stringCpy (inLine,"\r\n\r\n");
  write (gSd,string (inLine),stringLen (inLine));
  return 0;
}

int cgiPost (char *url,int itemCount,char **items,char **values) {
  /**
     Prepare fetching the document addressed by 'URL'
     using the HTTP POST method.<br>
     Precondition: optional: cgiSendCookieAdd()
     Postcondition: cgiRecv() can be called
                    warnCount()/warnReport() may be increased/show messsage
     @param[in] url - the UTL to post to
     @param[in] itemCount - number of pairs of item/value
     @param[in] items - items to be sent as item=value pairs delimitted by &
     @param[in] values - values to be sent as item=value pairs delimitted by &
     @param[out] url - contents destroyed
     @return 0 if ok, else a number > 0
  */

  int port;
  int ret;
  static Stringa inLine = NULL;
  static Stringa encodeBuff = NULL;
  static Stringa buffer = NULL;
  int i;

  urlParse (url);
  if (urlGetPart (URLPORT) == NULL)
    port = 0;
  else
    port = atoi (urlGetPart (URLPORT));
  if ((ret = httpOpen (urlGetPart (URLHOST),port)) != 0) {
    warnAdd ("cgiPost","Could not open TCPIP connection");
    return ret;
  }
  stringCreateOnce (inLine,100);
  stringPrintf (inLine,"POST %s HTTP/1.0\r\n",urlGetPart (URLPROG));
  write (gSd,string (inLine),stringLen (inLine));
  if (urlGetPart (URLUSER)) {
    stringPrintf (inLine,"Authorization: basic %s\r\n",
                  toBase64 (urlGetPart (URLUSER)));
    write (gSd,string (inLine),stringLen (inLine));
  }
  stringCpy (inLine,"User-Agent: http.c\r\n");
  write (gSd,string (inLine),stringLen (inLine));
  stringCpy (inLine,"Accept: */*\r\n");
  write (gSd,string (inLine),stringLen (inLine));
  stringPrintf (inLine,"Host: %s\r\n",urlGetPart (URLHOST));
  write (gSd,string (inLine),stringLen (inLine));
  if (gSendCookies != NULL)
    write (gSd,string (gSendCookies),stringLen (gSendCookies));
  stringCpy (inLine,"Content-type: application/x-www-form-urlencoded\r\n");
  write (gSd,string (inLine),stringLen (inLine));

  stringCreateClear (buffer,100);
  stringCreateClear (encodeBuff,100);
  for (i=0;i<itemCount;i++) {
    stringCat (buffer,items[i]);
    stringCat (buffer,"=");
    cgiEncodeWord (values[i],encodeBuff);
    stringCat (buffer,string (encodeBuff));
    if (i < itemCount-1)
      stringCat (buffer,"&");
  }
  stringPrintf (inLine,"Content-length: %ld\r\n\r\n",stringLen (buffer));
  write (gSd,string (inLine),stringLen (inLine));
  write (gSd,string (buffer),stringLen (buffer));
  return 0;
}

int cgiPostXML (char *url,char *xmlBuff) {
  /**
     Prepare fetching the document addressed by 'URL'
     using the HTTP POST method.<br>
     Precondition: optional: cgiSendCookieAdd().<br>
     Postcondition: cgiRecv() can be called;
                    warnCount()/warnReport() may be increased/show messsage
     @param[in] url - the URL to post to
     @param[in] xmlBuff - a SOAP message for example
     @param[out] url - contents destroyed
     @return 0 if ok, else a number > 0
  */
  int port;
  int ret;
  static Stringa inLine = NULL;

  urlParse (url);
  if (urlGetPart (URLPORT) == NULL)
    port = 0;
  else
    port = atoi (urlGetPart (URLPORT));
  if ((ret = httpOpen (urlGetPart (URLHOST),port)) != 0) {
    warnAdd ("cgiPost","Could not open TCPIP connection");
    return ret;
  }
  stringCreateOnce (inLine,100);
  stringPrintf (inLine,"POST %s HTTP/1.0\r\n",urlGetPart (URLPROG));
  write (gSd,string (inLine),stringLen (inLine));
  if (urlGetPart (URLUSER)) {
    stringPrintf (inLine,"Authorization: basic %s\r\n",
                  toBase64 (urlGetPart (URLUSER)));
    write (gSd,string (inLine),stringLen (inLine));
  }
  stringCpy (inLine,"User-Agent: http.c\r\n");
  write (gSd,string (inLine),stringLen (inLine));
  stringCpy (inLine,"Accept: */*\r\n");
  write (gSd,string (inLine),stringLen (inLine));
  stringPrintf (inLine,"Host: %s\r\n",urlGetPart (URLHOST));
  write (gSd,string (inLine),stringLen (inLine));
  if (gSendCookies != NULL)
    write (gSd,string (gSendCookies),stringLen (gSendCookies));
  stringCpy (inLine,"Content-type: text/xml\r\n");
  write (gSd,string (inLine),stringLen (inLine));
  stringPrintf (inLine,"Content-length: %ld\r\n\r\n",strlen (xmlBuff));
  write (gSd,string (inLine),stringLen (inLine));
  write (gSd,xmlBuff,strlen (xmlBuff));
  return 0;
}

int cgiPostMulti (char *url,int itemCount,
                  char **names,char **filenames,char **values) {
  /**
     Same as cgiPost but for multipart forms
  */
  int port;
  int ret;
  static Stringa inLine = NULL;
  static Stringa encodeBuff = NULL;
  static Stringa buffer = NULL;
  int i;
  static Stringa boundary = NULL;

  urlParse (url);
  if (urlGetPart (URLPORT) == NULL)
    port = 0;
  else
    port = atoi (urlGetPart (URLPORT));
  if ((ret = httpOpen (urlGetPart (URLHOST),port)) != 0) {
    warn ("cgiPstMulti: Could not open TCPIP connection");
    return ret;
  }
  stringCreateOnce (inLine,100);
  stringPrintf (inLine,"POST %s HTTP/1.0\r\n",urlGetPart (URLPROG));
  write (gSd,string (inLine),stringLen (inLine));
  if (urlGetPart (URLUSER)) {
    stringPrintf (inLine,"Authorization: basic %s\r\n",
                  toBase64 (urlGetPart (URLUSER)));
    write (gSd,string (inLine),stringLen (inLine));
  }
  if (gSendCookies != NULL)
    write (gSd,string (gSendCookies),stringLen (gSendCookies));
  stringCpy (inLine,"User-Agent: http.c\r\n");
  write (gSd,string (inLine),stringLen (inLine));
  stringCpy (inLine,"Accept: */*\r\n");
  write (gSd,string (inLine),stringLen (inLine));
  stringPrintf (inLine,"Host: %s\r\n",urlGetPart (URLHOST));
  write (gSd,string (inLine),stringLen (inLine));
  stringCreateOnce (boundary,100);
  stringPrintf (boundary,"-------------------------%d",getpid ());
  stringPrintf (inLine,"Content-type: multipart/form-data; boundary=%s\r\n",
                string (boundary));
  write (gSd,string (inLine),stringLen (inLine));

  stringCreateClear (buffer,100);
  stringCreateClear (encodeBuff,100);
  for (i=0;i<itemCount;i++) {
    stringCat (buffer,"--");
    stringCat (buffer,string (boundary));
    stringCat (buffer,"\r\nContent-Disposition: form-data; name=\"");
    stringCat (buffer,names[i]);
    stringCat (buffer,"\"");
    if (filenames[i] != NULL) {
      stringCat (buffer,"; filename=\"");
      stringCat (buffer,filenames[i]);
      stringCat (buffer,"\"");
    }
    stringCat (buffer,"\r\n\r\n");
    stringCat (buffer,values[i]);
    stringCat (buffer,"\r\n");
  }
  stringCat (buffer,"--");
  stringCat (buffer,string (boundary));
  stringCat (buffer,"--\r\n");
  stringPrintf (inLine,"Content-length: %ld\r\n\r\n",stringLen (buffer));
  write (gSd,string (inLine),stringLen (inLine));
  write (gSd,string (buffer),stringLen (buffer));
  return 0;
}

/// structure to be used in cgiPostMultiNoCache()
typedef struct {
  Stringa before; //!< part before the item
  char *content; //!< the item itself
  char *after; //!< part after the item
}Buffer;

int cgiPostMultiNoCache (char *url,int itemCount,
                         char **names,char **filenames,char **values) {
  /**
     Same as cgiPost but for multipart forms
  */
  int port;
  int ret;
  static Stringa inLine = NULL;
  static Stringa buffers = NULL;
  int i;
  static Stringa boundary = NULL;
  Buffer *currBuffer;
  long int len;

  urlParse (url);
  if (urlGetPart (URLPORT) == NULL)
    port = 0;
  else
    port = atoi (urlGetPart (URLPORT));
  if ((ret = httpOpen (urlGetPart (URLHOST),port)) != 0) {
    warn ("cgiPstMulti: Could not open TCPIP connection");
    return ret;
  }
  stringCreateOnce (inLine,100);
  stringPrintf (inLine,"POST %s HTTP/1.0\r\n",urlGetPart (URLPROG));
  write (gSd,string (inLine),stringLen (inLine));
  if (urlGetPart (URLUSER)) {
    stringPrintf (inLine,"Authorization: basic %s\r\n",
                  toBase64 (urlGetPart (URLUSER)));
    write (gSd,string (inLine),stringLen (inLine));
  }
  if (gSendCookies != NULL)
    write (gSd,string (gSendCookies),stringLen (gSendCookies));
  stringCpy (inLine,"User-Agent: http.c\r\n");
  write (gSd,string (inLine),stringLen (inLine));
  stringCpy (inLine,"Accept: */*\r\n");
  write (gSd,string (inLine),stringLen (inLine));
  stringPrintf (inLine,"Host: %s\r\n",urlGetPart (URLHOST));
  write (gSd,string (inLine),stringLen (inLine));
  stringCreateOnce (boundary,100);
  stringPrintf (boundary,"-------------------------%d",getpid ());
  stringPrintf (inLine,"Content-type: multipart/form-data; boundary=%s\r\n",
                string (boundary));
  write (gSd,string (inLine),stringLen (inLine));

  if (buffers != NULL) {
    for (i=0;i<arrayMax (buffers);i++) {
      currBuffer = arrp (buffers,i,Buffer);
      stringDestroy (currBuffer->before);
      currBuffer->content = NULL;
      currBuffer->after = NULL;
    }
    arrayClear (buffers);
  }
  else
    buffers = arrayCreate (2,Buffer);
  len = 0;
  for (i=0;i<itemCount;i++) {
    currBuffer = arrayp (buffers,arrayMax (buffers),Buffer);
    currBuffer->before = stringCreate (100);
    stringCat (currBuffer->before,"--");
    stringCat (currBuffer->before,string (boundary));
    stringCat (currBuffer->before,
               "\r\nContent-Disposition: form-data; name=\"");
    stringCat (currBuffer->before,names[i]);
    stringCat (currBuffer->before,"\"");
    if (filenames[i] != NULL) {
      stringCat (currBuffer->before,"; filename=\"");
      stringCat (currBuffer->before,filenames[i]);
      stringCat (currBuffer->before,"\"");
    }
    stringCat (currBuffer->before,"\r\n\r\n");
    len += stringLen (currBuffer->before);
    if (values[i] != NULL) {
      currBuffer->content = values[i];
      len += strlen (currBuffer->content);
    }
    else { // file
      len += hlr_fileSizeGet (filenames[i]);
    }
    currBuffer->after = "\r\n";
    len += strlen (currBuffer->after);
  }
  currBuffer = arrayp (buffers,arrayMax (buffers),Buffer);
  currBuffer->before = stringCreate (100);
  stringCat (currBuffer->before,"--");
  stringCat (currBuffer->before,string (boundary));
  stringCat (currBuffer->before,"--\r\n");
  len += stringLen (currBuffer->before);
  stringPrintf (inLine,"Content-length: %ld\r\n\r\n",len);
  write (gSd,string (inLine),stringLen (inLine));

  for (i=0;i<itemCount;i++) {
    currBuffer = arrayp (buffers,i,Buffer);
    write (gSd,string (currBuffer->before),stringLen (currBuffer->before));
    if (values[i] != NULL)
      write (gSd,currBuffer->content,strlen (currBuffer->content));
    else { // file
      FILE *fp;
      int n;
      int c;

      fp = fopen (filenames[i],"r");
      n = 0;
      stringClear (inLine);
      while ((c = fgetc (fp)) != EOF) {
        stringCatChar (inLine,c);
        n++;
        if (n == 1000) {
          write (gSd,string (inLine),stringLen (inLine));
          n = 0;
          stringClear (inLine);
        }
      }
      fclose (fp);
      if (n > 0)
        write (gSd,string (inLine),stringLen (inLine));
    }
    write (gSd,currBuffer->after,strlen (currBuffer->after));
  }
  currBuffer = arrayp (buffers,i,Buffer);
  write (gSd,string (currBuffer->before),stringLen (currBuffer->before));
  return 0;
}

static char *findBeginOfData (char *buff) {
  /**
     Find end of header / beginning of data in buff
     @param[in] buff - the buffer
     @return pointer to start of data, NULL if problem
  */
  char *cp;
  char c;
  char previousc;
  cp = buff - 1;
  previousc = '\0';
  while ((c = *++cp) != '\0') {
    if (c == '\r')
      continue;
    if (c == '\n' && previousc == '\n') {
      ++cp;
      break;
    }
    previousc = c;
  }
  if (c == '\0')
    return NULL;
  return cp;
}

char *cgiRecvWithHeaderLen (int *length) {
  /**
     Return the entire response from an http connection.<br>
     Precondition: successful cgi-Post/-PostMulti/-Get().<br>
     Postcondition: warnCount()/warnReport() may be increased/show messsage.
     @param[in] length - pointer to space for length of response,
                         NULL if not interested in it
     @param[out] length - now contains length of response
     @return a C string with the answer - complete, including response header
             memory is managed by this routine, value is valid until next call
             to any cgiRecv...() function
  */
  char buff[10000];
  static Stringa retBuff = NULL;
  int i;
  int len;

  if (gSd == -1)
    die ("cgiRecvWithHeaderLen without cgiPost/PostMulti/Get");
  stringCreateClear (retBuff,100);
  while ((len = read (gSd,buff,sizeof (buff))) > 0) {
    for (i=0;i<len;i++) {
      stringCatChar (retBuff,buff[i]);
    }
  }
  if (len != 0)
    warnAdd ("cgiRecvWithHeaderLen","read() problem");
  if (length != NULL)
    *length = stringLen (retBuff);
  // this in not yet clean: can data b binary ?

  if (!strStartsWithC (string (retBuff),"HTTP/1.0 200") &&
      !strStartsWithC (string (retBuff),"HTTP/1.1 200")) {
    static Stringa w = NULL;

    stringCreateOnce (w,100);
    stringPrintf (w,"unexpected response: %s",string (retBuff));
    warnAdd ("cgiRecvWithHeaderLen",string (w));
  }
  httpClose ();
  return string (retBuff);
}

static Stringa gHeaderBuf = NULL;

char *cgiRecvLen (int *length) {
  /**
     Receive from the http connection.<br>
     Precondition: successful cgi-Post/-PostMulti/-Get()
     Postcondition: warnCount()/warnReport() may be increased/show messsage
                    cgiRecvMimeType() can be called
     @param[in] length - pointer to space for length of response,
                         NULL if not interested in it
     @param[out] length - now contains length of response
     @return a C string with the answer - only data part without response header
             memory is managed by this routine, value is valid until next call
             to any cgiRecv...() function
  */
  char *data;
  char *response;
  if (gSd == -1)
    die ("cgiRecvLen without cgiPost/PostMulti/Get");
  stringCreateClear (gHeaderBuf,100);
  response = cgiRecvWithHeaderLen (length);
  data = findBeginOfData (response);
  if (data != NULL) {
    if (length > 0)
      *length = *length - (data - response);
    stringNCpy (gHeaderBuf,response,data - response);
  }
  else
    warnAdd ("cgiRecvLen","incomplete header");
  return data;
}

char *cgiRecvMimeType (void) {
  /**
     Get the mime type of a http response.<br>
     Precondition: successful call to cgiRecvLen() or cgiRecv()
     @return a C string with mimetype from the HTTP header received.
             Empty string if none found.
  */
  static char ContentType[] = "Content-type: ";
  static Stringa s = NULL;

  stringCreateClear (s,30);
  char *cp;
  char *cp2;
  if (gHeaderBuf == NULL)
    die ("cgiRecvMimeType() without cgiRecvLen()");
  if ((cp = strCaseStr (string (gHeaderBuf),ContentType)) != NULL) {
    cp += sizeof(ContentType)-1;
    if ((cp2 = strpbrk (cp,"\r\n;")) && cp2 - cp > 4)
      stringNCpy (s,cp,cp2-cp);
  }
  return string (s);
}

/// block len must be > 100
#define BLOCK_LEN 4096

int cgiRecvToFile (char *filename) {
  /**
     Receives response from http commection to file.<br>
     Precondition: successful cgi-Post/-PostMulti/-Get().<br>
     Note: can be used for very large files, since it
           does not use buffer the whole file in main memory.
     @param[in] filename - name of file to write the output to,
                           '-' for stdout
     @return number of bytes written to 'filename'
  */
  char buff[BLOCK_LEN];
  int len;
  char *cp;
  int fd; // file descriptor
  int lenTotal = 0;
  int headerLen;

  if (gSd == -1)
    die ("cgiRecvToFile without cgiPost/PostMulti/Get");
  // open output stream
  if (strEqual (filename,"-"))
    fd = 1; // stdout
  else {
    fd = open (filename,O_WRONLY | O_CREAT | O_TRUNC,0666);
    if (fd == -1)
      die ("cgiRecvToFile: open file %s failed (%d)",filename,errno);
  }
  // read first block and assume it contains the header
  memset (buff,0,BLOCK_LEN);
  len = read (gSd,buff,BLOCK_LEN);
  if (len < 12)
    die ("cgiRecvToFile: too short reponse (len=%d)",len);
  // check for proper HTTP protocol header in first block
  if (!strStartsWithC (buff,"HTTP/1.0 200") &&
      !strStartsWithC (buff,"HTTP/1.1 200")) {
    static Stringa w = NULL;

    stringCreateOnce (w,100);
    buff[BLOCK_LEN-1] = '\0';
    stringPrintf (w,"unexpected response: %s",buff);
    warnAdd ("cgiRecvToFile",string (w));
  }
  // find end of header / beginning of data --> cp
  cp = findBeginOfData (buff);
  if (cp == NULL)
    die ("cgiRecvToFile: incomplete header");
  headerLen = cp - buff;
  if (headerLen > len)
    die ("cgiRecvToFile: oops");
  // write (len - headerLen) bytes starting from cp
  if (write (fd,cp,len - headerLen) != len - headerLen)
    die ("cgiRecvToFile: write fail 1. block: %s",strerror (errno));
  lenTotal += len - headerLen;
  // copy the remaining blocks, if any
  while ((len = read (gSd,buff,BLOCK_LEN)) > 0) {
    if (write (fd,buff,len) != len)
      die ("cgiRecvToFile: write fail: %s",strerror (errno));
    lenTotal += len;
  }
  if (len != 0)
    die ("cgiRecvToFile: read error: %s",strerror (errno));
  // clean up
  httpClose ();
  if (fd != 1)
    close (fd);
  return lenTotal;
}

int cgiGetNTLM (char *url,char *user,char *passwordEnc,
                int numPair,char *items[],char *values[],
                Array result,
                Stringa mimeType) {
  /**
     POST items/values to url. Use user/password to login into url using NTLM.
     Result is delivered in 'result'.<br>
     Postcondition: problems reported in warnCount()/warnReport(), see file
                    log.c; e.g. if there was no result, status == -1 and
                    warnReport contains "No such file or directory"
     @param[in] url - the url to post to
     @param[in] user - user name
     @param[in] passwordEnc - if it starts with blank, it is assumed to be
                              encode and will be decoded
                              (using endec_decode1() from pwdecode.c)
     @param[in] numPair - number of elements in items/values
     @param[in] items - items to be posted, run in parallel with values
     @param[in] values - values to be posted, run in parallel with items
     @param[in] result - array of bytes, must exist
     @param[in] mimeType -- can be NULL, if not interested
     @param[out] result - filled (not '\0' terminated)
     @param[out] mimeType - mimeType received from url; NULL if not of interest
     @return 0 if OK, else non zero
  */
  static Stringa tmpFile = NULL;
  FILE *fp;
  int i;
  int len;
  char *cp;
  char *contents;
  static Stringa s = NULL;
  int status;

  stringCreateOnce (s,50);
  if (mimeType != NULL)
    die ("cgiGetNTLM: receiving mimeType not yet implemented.");
  if (url == NULL || user == NULL || passwordEnc == NULL || result == NULL)
    die ("cgiGetNTLM: at least one mandatory input parameter is NULL");
  if (*passwordEnc == ' ' && *(passwordEnc+1) != '\0')
    passwordEnc = endec_decode1 (passwordEnc+1);
  stringCreateOnce (tmpFile,30);
  stringPrintf (tmpFile,"/tmp/cgiGetNTLM_output.%d",getpid ());
  fp = PLABLA_POPEN ("/usr/bin/curl  --config -", "w");
  if (fp == NULL) {
    warnAdd ("cgiGetNTLM","popen failed");
    return -1;
  }
  fprintf (fp,"--ntlm\n");
  for (i=0;i<numPair;i++) {
    cgiEncodeWord (values[i],s);
    fprintf (fp,"--data %s=%s\n",items[i],string (s));
  }
  fprintf (fp,"--user %s:%s\n",user,passwordEnc);
  fprintf (fp,"--url %s\n",url);
  fprintf (fp,"--output %s\n",string (tmpFile));
  fprintf (fp,"--silent\n");
  status = pclose (fp);
  if (status != 0) {
    warnAdd ("cgiGetNTLM",stringPrintBuf ("curl failed, status=%d",status));
    return -2;
  }
  contents = hlr_fileRead (string (tmpFile),/*isText*/0,&len);
  if ((cp = hlr_fileRemove (string (tmpFile))) != NULL)
    if (!strstr (cp,"No such file"))
      die ("cgiGetNTLM: cannot remove %s (%s)",string (tmpFile),cp);
  if (contents == NULL)
    return -1;
  if (len < 1) {
    warnAdd ("cgiGetNTLM","empty result from fileRead");
    return -1;
  }
  array (result,len-1,char) = '\0';
  memcpy (result->base,contents,len);
  return 0;
}
