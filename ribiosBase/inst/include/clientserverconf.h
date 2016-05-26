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
/** @file clientserverconf.h
    for clientserver applications
*/
#ifndef CLIENTSERVERCONF_H
#define CLIENTSERVERCONF_H

/* define services offered; remember to register the ports
   in /etc/services, too
   per server S, six variables are defined:
   S_HOSTNAME   -- where the server process runs
   S_PORT       -- on which port is listens
   S_PROGDIR    -- in which directory the server executable is
   S_ADMINMAIL  -- eMail addresses of administrators
   S_LOGGROUP   -- unix group to give the log file to
   S_LOGDIR     -- directory in which to put the log file
*/

// default values for client/server applications
// BIM = bioinformatics manager (application account)
#define BIM_ADMINMAIL "bioinfoc@bioinfoc.ch"
#define BIM_LOGGROUP "bioinfoc"
#define BIM_LOGDIR "/data/log"
#define KILL_PROCESS_BY_NAME "/usr/bin/killall"

// GRPMAN - authorization server - production
#define GRPMAN_HOSTNAME "localhost"
#define GRPMAN_PORT 4664
#define GRPMAN_ADMINMAIL "bioinfoc@bioinfoc.ch"
#define GRPMAN_LOGGROUP "bioinfoc"
#define GRPMAN_LOGDIR "/data/log"

// NLP - nlp server
#define NLP_HOSTNAME "localhost"
#define NLP_PORT 4665

// TEST
#define TEST_HOSTNAME "localhost"
#define TEST_PORT 4660

// SAWI server
#define SAWIS_HOSTNAME "bioinfoc.ch"
#define SAWIS_PORT 4675

// ALCO server
#define ALCOS_HOSTNAME "bioinfoc.ch"
#define ALCOS_PORT 4681

// ONTO server
#define ONTOS_HOSTNAME "bioinfoc.ch"
#define ONTOS_PORT 4914

#endif
