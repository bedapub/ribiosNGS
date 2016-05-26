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
/** @file http.h
    @brief HTTP GET and POST stuff:
    core communication routines of a WWW browser.
    Module prefix cgi_
*/
#ifndef HTTP_H
#define HTTP_H

#ifdef __cplusplus
extern "C" {
#endif

#include "format.h"

extern void cgiProxySet (char *proxy,char *exception);
extern void cgiSendCookieReset (void);
extern void cgiSendCookieAdd (char *name,char *value);
extern int cgiSendCookiePresent (char *name);

extern char *toBase64 (char *in);
extern char *fromBase64 (char *in);

extern int cgiGet (char *url);
extern int cgiPost (char *url,int itemCount,char **items,char **values);
extern int cgiPostXML (char *url,char *xmlBuff);
extern int cgiPostMulti (char *url,int itemCount,
                         char **items,char **filenames,char **values);
extern int cgiPostMultiNoCache (char *url,int itemCount,
                                char **items,char **filenames,char **values);
extern char *cgiRecvWithHeaderLen (int *length);
/// Like cgiRecvWithHeaderLen if we are not interested in the length
#define cgiRecvWithHeader() cgiRecvWithHeaderLen(NULL)

extern char *cgiRecvLen (int *length);
/// Like cgiRecvLen if we are not interested in the length
#define cgiRecv() cgiRecvLen(NULL)

extern char *cgiRecvMimeType ();
extern int cgiRecvToFile (char *filename);

extern int cgiGetNTLM (char *url,char *user,char *passwordEnc,
                      int numPair,char *items[],char *values[],
                      Array result,Stringa mimeType);

#ifdef __cplusplus
}
#endif

#endif
