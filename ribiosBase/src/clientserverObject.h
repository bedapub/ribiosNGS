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
/** @file clientserverObject.h
    @brief TCP/IP client server functions.
    Module prefix: cso_
*/
/*
  In your program you usually need to include bios/conf/clientserverconf.h
  since it contains the site-wide valid port numbers
*/
#ifndef CLIENTSERVEROBJECT_H
#define CLIENTSERVEROBJECT_H

#ifdef __cplusplus
extern "C" {
#endif

#include "array.h"

/// Length of read buffer, should not be too short for efficiency reasons
#define CSO_READSBUFLEN 1024

/// th connection object
typedef struct _connection_ {
  int socket; //!< the socket
  Array writeBuf; //!< of char, the buffer for writing
  char readBuf[CSO_READSBUFLEN]; //!< the buffer for reading
  int readLen; //!< how many characters were received
  int readPos; //!< current position in the read buffer
}*Connection;
// do not access anything inside this struct from within your programs!

/// return status for receiver functions
#define CLSVNULL 0
/// return status for receiver functions
#define CLSVOK 1
/// return status for receiver functions
#define CLSVROGER -1
/// return status for receiver functions
#define CLSVFAIL 2

// for server only
extern void cso_serverCloseConnection (Connection conn);
extern void cso_serverCloseSocket (void);
extern void cso_serverSetLog (int doLogging);
extern void cso_serverSetLogLevel (int level);
extern void cso_setQueueLength (int len);
extern void cso_serverInit (unsigned int port);
extern Connection cso_waitForJob (void);
extern void cso_die (Connection conn,char *format,...);
extern void cso_serverEnd (void);
extern void cso_serverLoop (int port,void (*work)(Connection conn,char *comm),
                            int doFork);
extern void cso_serverRecvRoger(Connection conn);

// for client only
extern Connection cso_clientInit (char *hostname,unsigned int port);
extern void cso_clientEnd (Connection conn);
extern void cso_clientSetMsg (char *s);

// for both
extern void cso_destroyConnection_func (Connection conn);
/// do not use above function but macro cso_destroyConnection()
#define cso_destroyConnection(conn) (cso_destroyConnection_func(conn),conn=NULL)
extern void cso_closeConnection (Connection conn);
extern void cso_sendStr (Connection conn,char *s);
extern int  cso_recvStrA (Connection conn,Array s /* of char */);
extern int cso_recvStrF_func (Connection conn,char *s,int len);
/// do not use above function but macro cso_recvStrF()
#define cso_recvStrF(conn,s) cso_recvStrF_func(conn,s,sizeof(s))
extern int cso_recvStrV (Connection conn,char **s);
extern void cso_sendBinary (Connection conn,int len,char *s);
extern int  cso_recvBinary (Connection conn,Array s /* of bytes */);
extern void cso_sendInt (Connection conn,int i);
extern int  cso_recvInt (Connection conn,int *r);
extern void cso_sendRoger (Connection conn);

#ifdef __cplusplus
}
#endif

#endif
