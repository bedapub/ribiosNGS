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
/** @file hlrclock.h
    @brief Mininal and accurate time measurement.
    Module prefix hlr_
*/
#ifndef HLRCLOCK
#define HLRCLOCK

#ifdef __cplusplus
extern "C" {
#endif

#include <stdlib.h>
#include <time.h>

extern void hlr_startClock (void);
extern float hlr_readClock (void);

/// Clock is a pointer to the system cock_t
typedef clock_t *Clock;

extern Clock hlr_clockCreate (void);
extern void hlr_clockReset (Clock this1);
extern float hlr_clockRead (Clock this1);
/// Destroy the Clock instance
#define hlr_clockDestroy(this1) (free (this1),this1=0)

extern char *hlr_YYYYMMDDHHMM (time_t t);
extern char *hlr_YYYYMMDDHHMMSS (time_t t);
extern char *hlr_getTime (void);
extern char *hlr_DDMMYYYY (void);

extern int hlr_date2time (char *date,struct tm *time_strp);
extern void hlr_dateAdd (char *date,int minutes);
extern int hlr_dateDiff (char *date1,char *date2);

#ifdef __cplusplus
}
#endif

#endif
