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
/** @file identificatorconf.h
    site-specific configuration for identificator.c
    documentation: see bios/src/libdev/identificator.c
*/
#ifndef IDENTIFICATORCONF_H
#define IDENTIFICATORCONF_H

// 1 = use UNIX authentication, 0 = don't use it
#define IDENTIFICATOR_USE_UNIX 0

// 1 = use Pluggable Authentication Module (PAM), 0 = PAM not available
#define IDENTIFICATOR_USE_PAM 0

// 1 = use usrman, 0 = usrman not available
#define IDENTIFICATOR_USE_USRMAN 1
#define USRMAN_CHECK "/apps/bin/usrman_check"
/* use '#define USRMAN_CHECK NULL' to disable
   usrman checking at this site */

#define IDENTIFICATOR_LOG "/data/log/identificator.log"

#endif
