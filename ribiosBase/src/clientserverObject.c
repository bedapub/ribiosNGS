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
/** @file clientserverObject.c
    @brief TCP/IP client server functions.
    Module prefix: cso_
*/
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/times.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>
#include <errno.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/signal.h>

#include "log.h"
#include "format.h"
#include "hlrmisc.h"
#include "hlrclock.h"
#include "clientserverObject.h"

/// ASCII value of Esc
#define ESC '\033'

static struct sockaddr_in sin;
static int serverSd = -1; // socket descriptor number of receiving socket
static int gJobCnt = 0;
static int gQueueLen = 10; // for example

static Connection createConnection (void) {
  Connection conn;

  conn = (Connection)hlr_calloc (1,sizeof (struct _connection_));
  conn->socket = -1;
  conn->writeBuf = arrayCreate (100,char);
  conn->readLen = 0;
  conn->readPos = 0;
  return conn;
}

/* ------------ routines & memory for the server only : ----- */

static int gIsLogging = 0;
static int gLogLevel = 1;
static float jobTimeTotal = 0.0;
static float jobTimeMax = 0.0;

void cso_serverCloseConnection (Connection conn) {
  /**
     Close connection for parent process of parallelized server
     @param[in] conn - the connection
  */
  if (close (conn->socket) == -1)
    perror ("cso_serverCloseConnection");
}

void cso_serverCloseSocket (void) {
  /**
     Close socket for child process of parallelized server
  */
  if (close (serverSd) == -1)
    perror ("cso_serverCloseSocket: close");
}

void cso_serverSetLog (int doLogging) {
  /**
     Turn loggn on or off
     @param[in] doLogging - 1: turn logging on (prints text to stderr, see
                            log.h; 0: no logging
  */
  gIsLogging = doLogging;
}

void cso_serverSetLogLevel (int level) {
  /**
     Set the level of logging.<br>
     Note: This routine has only an effect, if logging is enabled using
           cso_serverSetLog (1)
     @param[in] level - 0: only startup and shutdown messages
                        1: messages per request, too  (this is the default)
  */
  gLogLevel = level;
}

void cso_setQueueLength (int len) {
  /**
     Set how many clients cn connect at the same time.<br>
     Postcondition: cso_serverInit can be called.<br>
     @param[in] len - server can queue up to len requests (default is 10);
                      clients trying to connect when queue is full will be
                      refused
  */
  gQueueLen = len;
}

void cso_serverInit (unsigned int port) {
  /**
     Initialized the server to listen to a port
     @param[in] port - the port to listen to
  */
  if ((serverSd = socket (AF_INET,SOCK_STREAM,0)) == -1) {
    REprintf("socket");
    return;
  }
  memset (&sin,0,sizeof (sin));
  sin.sin_family = AF_INET;
  sin.sin_addr.s_addr = INADDR_ANY;
  sin.sin_port = htons (port);
  if (bind (serverSd,(struct sockaddr*)&sin,sizeof (sin)) == -1) {
    logWrite ("check port %d\n",port);
    REprintf("bind");
    return;
  }
  if (listen (serverSd,gQueueLen) == -1) {
    REprintf("listen");
    return;
  }
  if (gIsLogging)
    logWriteT ("server starts listening on port %u (0x%X).\n",port,port);
}

Connection cso_waitForJob (void) {
  /**
     Waits until client initiates a job or signal to cease received
     @return new connection if job received,
             NULL if problem occured
  */
  Connection conn;

  conn = createConnection ();
  if (gIsLogging && gJobCnt > 0) {
    float jobTime;
    jobTime = hlr_readClock ();
    jobTimeTotal += jobTime;
    if (jobTime > jobTimeMax)
      jobTimeMax = jobTime;
    if (gLogLevel > 0)
      logWrite ("cso_waitForJob: job %d finished. cpu=%.2fs/%.2fs/%.2fs/%.2fs this/avg/max/total #arrays=%d #open_mallocs=%d.\n",
                gJobCnt,jobTime,jobTimeTotal/gJobCnt,jobTimeMax,jobTimeTotal,
                arrayNumber (),hlr_getAllocCnt ());
  }
  if ((conn->socket = accept (serverSd,0,0)) == -1) {
    perror ("cso_waitForJob: accept");
    return NULL;
  }
  arrayClear (conn->writeBuf);
  if (gIsLogging) {
    gJobCnt++;
    if (gLogLevel > 0)
      logWriteT ("cso_waitForJob: job arrived.\n");
    hlr_startClock ();
  }
  return conn;
}

/// if the message is longer the server will crash in unpredictable ways !!!
#define ERR_BUFF_LEN 100000

void cso_die (Connection conn,char *format,...) {
  /**
     Used by the server to signal to the client that the current operation
     fatally failed.<br>
     Note: usage analogous to printf ()
     @param[in] conn - the connection
     @param[in] format - format for the message
  */
  va_list args;
  char errbuf[ERR_BUFF_LEN];
  int i;

  if (gIsLogging)
    logWriteT ("cso_die: server requests client abort.\n");
  va_start (args,format);
  errbuf[ERR_BUFF_LEN-1] = '\0';
  vsprintf (errbuf,format,args);
  if (errbuf[ERR_BUFF_LEN-1] != '\0')
    die ("buffer overflow in cso_die()");
  va_end (args);

  array (conn->writeBuf,arrayMax (conn->writeBuf),char) = ESC;
  array (conn->writeBuf,arrayMax (conn->writeBuf),char) = 'D';
  i = arrayMax (conn->writeBuf); // first free position
  array (conn->writeBuf,i + strlen (errbuf),char) = '\0'; // allocate
  strcpy (arrp (conn->writeBuf,i,char),errbuf); // including the null char
  cso_sendRoger (conn);
  if (gIsLogging && strlen (errbuf)) {
    if (errbuf[strlen (errbuf)-1] != '\n')
      strcat (errbuf,"\n");
    logWrite (errbuf);
  }
}

void cso_serverEnd (void) {
  /**
     Shuts down the server.<br>
     Precondition: cso_serverInit()
  */
  if (gIsLogging) {
    logWriteT ("server shutdown after %d jobs.\n",gJobCnt);
    logWrite ("cpu=%.2fs/%.2fs/%.2fs avg/max/total #arrays=%d #open_mallocs=%d.\n",
              jobTimeTotal/gJobCnt,jobTimeMax,jobTimeTotal,
              arrayNumber (),hlr_getAllocCnt ());
  }
  if (serverSd != -1) {
    if (shutdown (serverSd,SHUT_RDWR) == -1) {
      if (errno != ENOTCONN)
        perror ("cso_serverEnd: serverSd: shutdown");
      /* the client sends roger and immediately disconnects,
         so the 'ENOTCONN' (Transport endpoint is not connected)
         is the result to expect. */
    }
    else
      romsg ("INFO: cso_serverEnd: serverSd: shutdown succeeded");
    if (close (serverSd) == -1)
      perror ("cso_serverEnd: serverSd: close");
    serverSd = -1;
  }
}

void cso_serverLoop (int port,void (*work)(Connection conn,char *comm),
                     int doFork) {
  /**
     Calls the function that executes the main loop of a server.<br>
     Precondition: cso_serverSetLog(),cso_serverSetLogLevel(),
                   cso_sserverSetLogLevel() may be called before this
                   function<br>
     Postcondition: work() has been called for each job that arrived from
                    some client.
     @param[in] port - TCP/IP port to listen at
     @param[in] work - function to execute when a job (except for shutdown)
                       arrives. This function should implment the command
                       'check' that sends 1 if the server is ok, else 0
     @param[in] doFork - if 1, each work() will run in a separate process
                         if 0, work() is done in the process running
                         cso_serverLoop()
  */
  /*
    Implements a server with the following assumptions:
    - the client first sends a string containing the server
      command to be executed. The special command 'shutdown' is
      implemented by this routine; other commands are to be
      implemented by the user-supplied function work().
    - depending on the command 'comm', function work() may
      request more data from connection 'conn' (e.g. using cso_recvStrV(),
      as defined by the application's protocol).
    - function work() writes results using e.g. cso_sendStr();
      it does not use cso_sendRoger()
    - after the function work() returns, this routine sends the
      ROGER signal to the client and terminates the client connection.
    Note: the capability of the roger protocol for several client/server
          roundtrips within one connection is not used by this server loop.
  */
  char *cmd = NULL;
  Connection conn;

  if (work == NULL)
    die ("cso_serverLoop() with NULL work()");
  cso_serverInit (port);
  while ((conn = cso_waitForJob ()) != NULL) {
    if (cso_recvStrV (conn,&cmd) != CLSVOK || cmd == NULL) {
      cso_die (conn,"no command");
      cso_closeConnection (conn);
      cso_destroyConnection (conn);
      continue;
    }
    if (strEqual (cmd,"shutdown")) {
      if (cso_recvStrA (conn,NULL) != CLSVROGER)
        cso_die (conn,"ROGER expected");
      cso_closeConnection (conn);
      cso_destroyConnection (conn);
      break;
    }
    if (doFork) {
      if (fork ()) { // parent process
        cso_serverCloseConnection (conn);
        cso_destroyConnection (conn);
        waitpid (-1,NULL,WNOHANG);
        continue;
      }
      else // child process
        work (conn,cmd);
    }
    else
      work (conn,cmd);
    //cso_sendRoger (conn); // (deliver results to client) let the implementation of the loop do it
    cso_closeConnection (conn);
    cso_destroyConnection (conn);
    if (doFork) { // can only happen in child process
      cso_serverCloseSocket ();
      REprintf("can only happen in child process");
    }
  }
  cso_serverEnd ();
}

void cso_serverRecvRoger (Connection conn) {
  /**
     Expect to receive ROGER, force client to cso_die()
     if it violates application's protocol
     @param[in] conn - connection obtained from cso_waitForJob()
  */
  int r = cso_recvStrA (conn,NULL);
  if (r != CLSVROGER)
    cso_die (conn,"cso_recvRoger: ROGER (%d) expected, but got %d.",
             CLSVROGER,r);
}

/* ------------ routines for the client only : ------------- */

static char *gClientMsg = "+";

Connection cso_clientInit (char *hostname,unsigned int port)  {
  /**
     Open a connection to the server.<br>
     Precondition: an error message can only returned if
                   cso_clientSetMsg(NULL) is in effect<br>
     Postcondition: terminates process if illegal use of this module<br>
     Note: incompatible with strtok() on SGI IRIX 6.2
           on LINUX RedHat (2.4.14), gethostbyname() fails if
           called very often
     @param[in] hostname - e.g. bioinfo.roche.com
     @param[in] port - e.g. 1234
     @return a connection
  */
  static struct hostent *hp = NULL;
  Connection conn;
  static char *prevHostname = NULL;

  conn = createConnection ();
  if (!(prevHostname != NULL && hp != NULL &&
        strEqual (prevHostname,hp->h_name))) {
    if ((hp = gethostbyname (hostname)) == NULL)
      die ("cso_clientInit: gethostbyname(%s) failed",hostname);
    strReplace (&prevHostname,hp->h_name);
  }
  memset (&sin,0,sizeof (sin));
  sin.sin_family = AF_INET;
  sin.sin_addr.s_addr = ((struct in_addr *) (hp->h_addr))->s_addr;
  sin.sin_port = htons (port);
  if ((conn->socket = socket (AF_INET,SOCK_STREAM,0)) == -1)
    die ("cso_clientInit: socket: %s\n%s",strerror (errno),gClientMsg);
  if (connect (conn->socket,(struct sockaddr*)&sin,sizeof (sin)) == -1) {
    if (!gClientMsg)
      return NULL;
    if (*gClientMsg == '+') {
      warn ("cso_clientInit: connect: %s\nThe server %s:%d is probably not running.\n",
            strerror (errno),hostname,port);
      if (gClientMsg[1] != '\0')
        printf ("%s",gClientMsg+1);
    }
    else
      warn (gClientMsg);
    REprintf("Faiulure in cso_clientInit - please contact the developer");
  }
  return conn;
}

void cso_clientEnd (Connection conn) {
  /**
     Ends a client.<br>
     Precondition: cso_clientInit()
     @param[in] conn - the connection
  */
  if (conn == NULL)
    die ("cso_clientEnd: no connection");
  if (conn->socket == -1)
    die ("cso_clientEnd: nothing to be ended");
  else
    cso_closeConnection (conn);
  cso_destroyConnection (conn);
}

void cso_clientSetMsg (char *s) {
  /**
     Installs a message to be displayed when the client crashes
     during startup.<br>
     Postcondition: if s is NULL, cso_clientInit() will not die() on
                    a connection fault, but return an error message.<br>
     Note: if this routine is not called, a default message is displayed.
     Note: you may change the contents of 's' (if you have set it) during the
           course of the program, but not free it.
     @param[in] s - NULL = suppress messages, don't die in cso_clientInit();
                    "+msg" = system default message + user message 'msg';
                    "msg"  = only user message (not starting with '+');
                    default is "+" (= system message only).
  */
  gClientMsg = s;
}

/* ------------ routines for both client and server ---------- */

void cso_destroyConnection_func (Connection conn) {
  /**
     Destroys a connection
     @param[in] conn - the connection
  */
  arrayDestroy (conn->writeBuf);
  hlr_free (conn);
}

void cso_closeConnection (Connection conn) {
  /**
     Closes a connection
     @param[in] conn - the connection
  */
  if (conn->socket != -1) {
    if (shutdown (conn->socket,SHUT_RDWR) == -1)
      perror ("cso_closeConnection: server process socket: shutdown");
    if (close (conn->socket) == -1)
      perror ("cso_closeConnection: server process socket: close");
    conn->socket = -1;
  }
}

/* ----- mini-module for buffered READing from a Socket
   readc ()     -- get one char (efficient), -1 if end of read
   readBlock () -- internal routine
*/

static int readBlock (Connection conn) {
  conn->readLen = recv (conn->socket,conn->readBuf,CSO_READSBUFLEN,0);
  if (conn->readLen == -1) {
    perror ("readBlock");
    return EOF;
  }
  if (conn->readLen == 0)
    return EOF;
  conn->readPos = 0;
  return conn->readBuf[conn->readPos];
}

/// macro to simplify code
#define readsc(conn) (++conn->readPos >= conn->readLen ? readBlock(conn) : conn->readBuf[conn->readPos])

/* --------- end of 'read' minimodule ---------- */

void cso_sendStr (Connection conn,char *s) {
  /**
     Sends a string across the connection
     @param[in] conn - the connection
     @param[in] s - string to send or NULL; differentiates between NULL
                    and empty string
  */
  int i;
  if (s == NULL) {
    array (conn->writeBuf,arrayMax (conn->writeBuf),char) = ESC;
    array (conn->writeBuf,arrayMax (conn->writeBuf),char) = '0';
    return;
  }
  if (*s == ESC)
    array (conn->writeBuf,arrayMax (conn->writeBuf),char) = ESC;
  i = arrayMax (conn->writeBuf); // first free position
  array (conn->writeBuf,i + strlen(s),char) = '\0'; // allocate
  strcpy (arrp (conn->writeBuf,i,char),s); // including the null
}

int cso_recvStrA (Connection conn,Array s) {
  /**
     Expects to receive a string or 'Roger'
     @param[in] conn - the connection
     @param[in] s - Array of char
                    or NULL, if the string expected is not of interest
     @param[out] s - contains a null-terminated string filling
                     exactly the Array s, i.e. the following holds:
                     arru (s,arrayMax (s)-1,char) == '\0';
     @return CLSVOK - if non-null string received; output s is valid;
             CLSVNULL - if null value received; output s contains "";
                        (note: the empty string is not null)
             CLSVROGER - if roger received; output s is unchanged;
             CLSVFAIL - if error occured, e.g. tcp/ip connection interrupted;
  */
  int c;
  int amax;
  int pos = -1;
  int doDie = 0;
  static Array recvBuf = NULL; // in case result is not of interest, use this buffer
  if (s == NULL) {
    if (!recvBuf)
      recvBuf = arrayCreate (10,char);
    s = recvBuf;
  }
  c = readsc (conn);
  if (c < 0) {
    warn ("cso_recvStrA: unexpected EOF (first char)");
    array (s,0,char) = '\0';
    return CLSVFAIL;
  }
  if (c == ESC) {
    c = readsc (conn);
    if (c < 0) {
      warn ("cso_recvStrA: unexpected EOF after ESC");
      array (s,0,char) = '\0';
      return CLSVFAIL;
    }
    if (c == 'R')
      return CLSVROGER;
    else if (c == 'D')
      doDie = 1;
    else if (c == '0') {
      stringClear (s);
      return CLSVNULL;
    }
    else if (c == ESC)
      ;
    else
      die ("cso_recvStrA: expected type String, but received %c (%d)",c,c);
  }
  /* the following section uses a dynamic buffer allocation scheme
     basing on the Array module, but not involving a function call
     for adding each character; this is avoided by keeping a copy of
     arrayMax and a counter for the insert position, so I can
     know when the array would overflow; I leave the actual
     extension and memory reallocation to the Array module.
     Just for speed
  */
  array (s,++pos,char) = c;
  amax = arrayMax (s);
  if (c != '\0')
    while ((c = readsc (conn)) != EOF) {
      if (++pos >= amax) {
        array (s,amax+300,char) = '\0';
        amax = arrayMax (s);
      }
      arru (s,pos,char) = c;
      if (c == '\0')
        break;
    }
  if (c == EOF)
    warn ("cso_recvStrA: EOF before end of string");
  if (c != '\0')
    die ("cso_recvStrA: unterminated string");
  if (doDie) {
    cso_closeConnection (conn);
    cso_serverEnd ();
    die ("cso_recvStrA: peer requested abort: %s",string (s)+1);
  }
  arraySetMax (s,pos+1);
  if (arrayMax (s) <= 0 || array (s,arrayMax (s)-1,char))
    die ("cso_recvStrA: stringCheck fails");
  return 1;
}

int cso_recvStrF_func (Connection conn,char *s,int len) {
  /**
     Receive string of maximum len bytes (including the trailing '\0')
     @param[in] conn - the connection
     @param[out] s - null terminated string
     @param[in] len - maximum number of bytes to receive
     @return CLSVOK - s has new value;
             CLSVROGER;
             CLSVNULL - s has empty string
  */
  static Array as = NULL;
  int r;
  if (s == NULL || len <= 0)
    die ("cso_recvStrF_func: s/len");
  if (as == NULL)
    as = arrayCreate (10,char);
  r = cso_recvStrA (conn,as);
  if (r == CLSVNULL)
    s[0] = '\0';
  else if (r == CLSVOK) {
    s[len-1] = '\0';
    strncpy (s,string (as),len-1);
    if (len < stringLen (as))
      warn ("cso_recvStrF_func: received string of length %d, but expected only %d",
            stringLen (as),len-1);
  }
  return r;
}

int cso_recvStrV (Connection conn,char **s) {
  /**
     Receive string of variable length.
     Like cso_recvStrA, but with pointer to string.
     Don't forget to hlr_free() the result after use.
     @param[in] conn - the connection
     @param[in] s - pointer to a place for storing a char*
                    if *s is non-NULL, *s is free()ed
     @param[out] *s - '\0', if NULL string received, else
                      pointer to string; the memory of this
                      string belongs to the user of this routine
     @return CLSVOK or CLSVROGER or CLSVFAIL
  */
  static Array as = NULL;
  int r;

  if (s == NULL)
    die ("cso_recvStrV: NULL");
  if (as == NULL)
    as = arrayCreate (10,char);
  hlr_free (*s);
  r = cso_recvStrA (conn,as);
  if (r == CLSVNULL)
    return CLSVOK;
  if (r == CLSVFAIL)
    return CLSVFAIL;
  if (r == CLSVOK)
    *s = hlr_strdup (string (as));
  return r;
}

/// number of bytes in integer
#define INTLEN 4

void cso_sendBinary (Connection conn,int len,char *s) {
  /**
     Sends binary data across the connection
     @param[in] conn - the connection
     @param[in] len - length of the data
     @param[in] s - the data
  */
  int i;
  char *cp = (char*)&len;
  if (sizeof (int) != INTLEN)
    die ("cso_sendBinary: integer length is not %d, recompile code",INTLEN);
  array (conn->writeBuf,arrayMax (conn->writeBuf),char) = ESC;
  array (conn->writeBuf,arrayMax (conn->writeBuf),char) = 'B';
  for (i=0;i<INTLEN;++i)
    array (conn->writeBuf,arrayMax (conn->writeBuf),char) = cp[i];
  if (len < 0)
    die ("cso_sendBinary: len=%d",len);
  if (len == 0)
    return;
  i = arrayMax (conn->writeBuf); // first free position
  array (conn->writeBuf,i+len-1,char) = '\0'; // allocate
  memcpy (arrp (conn->writeBuf,i,char),s,len);
}

int cso_recvBinary (Connection conn,Array s /* of bytes */) {
  /**
     Receives binary data.<br>
     Note: this function has not been implemented because there was no need ever
     @param[in] conn - the connection
     @param[in] s - an existing Array of bytes
     @param[out] s - filled with binary data received
  */
  die ("cso_recvBinary: not implemented");
  return 0;
}

void cso_sendInt (Connection conn,int i) {
  /**
     Sends an int across the connection.
     @param[in] conn - the connection
     @param[in] i - the int
  */
  static char is[HLR_ITOA_SIZE];
  sprintf (is,"%d",i);
  cso_sendStr (conn,is);
}

int cso_recvInt (Connection conn,int *r) {
  /**
     Expects to receive an integer or Roger
     @param[in] conn - the connection
     @param[out] *r - the int but only if not Roger
     @return CLSVOK if integer received, CLSVROGER if roger received
  */
  static Array is = NULL;
  int rc;
  if (is == NULL)
    is = arrayCreate (40,char);
  if ((rc = cso_recvStrA (conn,is)) == CLSVOK)
    *r = atoi (string (is));
  else if (rc == CLSVNULL)
    die ("cso_recvInt: null received");
  return rc;
}

void cso_sendRoger (Connection conn) {
  /**
     Sends the data accumulated via the send calls in sendbuf.<br>
     Precondition: cso_waitForJob() or cso_initClient()
     @param[in] conn - the connection
  */
  int i;
  Array wb = conn->writeBuf;

  if (wb == NULL)
    die ("cso_sendRoger: precondition not met. call smth. before.");
  array (wb,arrayMax (wb),char) = ESC;
  array (wb,arrayMax (wb),char) = 'R';
  i = send (conn->socket,string (wb),arrayMax (wb),0);
  if (i != arrayMax (wb))
    die ("cso_sendRoger: send: %s",strerror (errno));
  arraySetMax (wb,0);
}
