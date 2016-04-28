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
/** @file hlrclock.c
    @brief Mininal and accurate time measurement.
    Module prefix hlr_
*/
#include "plabla.h"
#include PLABLA_INCLUDE_IO_UNISTD
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <sys/types.h>
#if BIOS_PLATFORM == BIOS_PLATFORM_IRIX || BIOS_PLATFORM == BIOS_PLATFORM_SOLARIS || BIOS_PLATFORM == BIOS_PLATFORM_LINUX
#include <sys/times.h>
#endif
#include <time.h>
#include "log.h"
#include "hlrclock.h"

/* ----------------- measure execution time --------- */

static struct tms dummytms;
static clock_t startTime;

/* part1: just one clock instance */

/*
  Usage:
  #include <stdio.h>
  #include "hlrclock.h"
  hlr_startClock ();
  for (int i=1;i<10000000;i++);
  printf ("elapsed: %.2f seconds.\n",hlr_readClock ());
*/

void hlr_startClock (void) {
  /**
     Start the clock.
  */
  startTime = times (&dummytms);
}

float hlr_readClock (void) {
  /**
     Returns elapsed time since last hlr_startClock() in seconds
     @return seconds since last hlr_startClock()
  */
  long elapsed = times (&dummytms) - startTime;
  return (float)elapsed / (float)sysconf (_SC_CLK_TCK);
}

/* part2: manage several clocks */
/*
  Clock c1 = hlr_clockCreate ();
  Clock c2 = hlr_clockCreate ();
  Clock c3 = hlr_clockCreate ();
  printf ("start clock1, 2 and 3; wait ...\n");
  for (i=1;i<10000000;i++);
  printf ("elapsed on clock1: %.2f seconds.\n",hlr_clockRead (c1));
  printf ("wait ...\n");
  for (i=1;i<10000000;i++);
  printf ("read clock1, 2 and 3\n");
  printf ("elapsed on clock1: %.2f seconds.\n",hlr_clockRead (c1));
  printf ("elapsed on clock2: %.2f seconds.\n",hlr_clockRead (c2));
  printf ("elapsed on clock3: %.2f seconds.\n",hlr_clockRead (c3));
  printf ("reset clock3\n");
  hlr_clockDestroy (c1);
  hlr_clockReset (c3);
  printf ("wait ...\n");
  for (i=1;i<10000000;i++)
    ;
  printf ("read clock2 and 3\n");
  printf ("elapsed on clock2: %.2f seconds.\n",hlr_clockRead (c2));
  printf ("elapsed on clock3: %.2f seconds.\n",hlr_clockRead (c3));
*/

void hlr_clockReset (Clock this1) {
  /**
     Resets the clock.
     @param[in] this1 - the Clock
  */
  *this1 = times (&dummytms);
}

Clock hlr_clockCreate (void) {
  /**
     Creates a Clock instance
  */
  Clock this1 = (Clock)malloc (sizeof (clock_t));
  hlr_clockReset (this1);
  return this1;
}

float hlr_clockRead (Clock this1) {
  /**
     Reads the clock.
     @param[in] this1 - the Clock
     @return seconds since last hlr_clockCreate() or hlr_clockReset()
  */
  long elapsed = times (&dummytms) - *this1;
  return (float)elapsed / (float)sysconf (_SC_CLK_TCK);
}

char *hlr_YYYYMMDDHHMM (time_t t) {
  /**
     Reformat t (of type time_t) into the form:<br>
     1234567890123456<br>
     1997-07-16_19:06<br>
     0123456789012345<br>
     The memory returned is managed by this routine;
     it is r/w for the user, but may not be freed or realloced.<br>
     It stays stable until the next call to thise routine.
  */
  static char niceDate[17];
  struct tm *tmp;
  tmp = localtime (&t);
  strftime (niceDate,sizeof (niceDate),"%Y-%m-%d_%H:%M",tmp);
  return niceDate;
}

char *hlr_YYYYMMDDHHMMSS (time_t t) {
  /**
     Reformat t (of type time_t) into the form:<br>
     1234567890123456789<br>
     1997-07-16_19:06:58<br>
     0123456789012345678<br>
     The memory returned is managed by this routine;
     it is r/w for the user, but may not be freed or realloced.<br>
     It stays stable until the next call to thise routine.
  */
  static char niceDate[20];
  struct tm *tmp;
  tmp = localtime (&t);
  strftime (niceDate,sizeof (niceDate),"%Y-%m-%d_%H:%M:%S",tmp);
  return niceDate;
}

char *hlr_getTime (void) {
  /**
     Look at the wall clock: 1997-07-16_19:06
  */
  return (hlr_YYYYMMDDHHMM (time (NULL)));
}

char *hlr_DDMMYYYY (void) {
  /**
     Get current date<br>
     12345678901<br>
     01-OCT-1962<br>
  */
  static char niceDate[12];
  time_t t;
  struct tm *tmp;
  t = time (NULL);
  tmp = localtime (&t);
  strftime (niceDate,sizeof (niceDate),"%d-%b-%Y",tmp);
  return niceDate;
}

static int hlr_date2time_d (char *date,struct tm *time_strp) {
  /**
     Convert date into a UNIX C library tm struct.
     @param[in] date - in the form '1997-07-16_00:00'
     @param[in] time_strp - location for storing the result
                            or NULL if not interested in result
     @param[out] date - contents destroyed
     @param[out] time_strp - filled with equivalent of 'date' (if not NULL):<br>
                             1234567890123456<br>
                             1997-07-16_19:06<br>
                             0123456789012345
     @return # seconds since 00:00:00 UTC, January 1, 1970
  */
  struct tm time_str;
  int sec0;

  if (strlen (date) != 16 || date[4] != '-' || date[7] != '-' ||
      date[10] != '_' || date[13] != ':')
    die ("hlr_date2time: ill-formatted input: %s",date);
  date[4] = '\0';
  date[7] = '\0';
  date[10] = '\0';
  date[13] = '\0';
  if (time_strp == NULL)
    time_strp = &time_str;
  time_strp->tm_year = atoi (date+0) - 1900;
  time_strp->tm_mon = atoi (date+5) - 1;
  time_strp->tm_mday = atoi (date+8);
  time_strp->tm_hour = atoi (date+11);
  time_strp->tm_min = atoi (date+14);
  time_strp->tm_sec = 0;
  time_strp->tm_isdst = -1;
  if ((sec0 = mktime (time_strp)) == -1)
    die ("hlr_date2time: mktime failed");
  return sec0;
}

int hlr_date2time (char *date,struct tm *time_strp) {
  /**
     Convert date into a UNIX C library tm struct.<br>
     Like static hlr_date2time_d(), but preserves its input
     @param[in] date - in the form '1997-07-16_00:00'
     @param[in] time_strp - location for storing the result
                            or NULL if not interested in result
     @param[out] date - contents destroyed
     @param[out] time_strp - filled with equivalent of 'date' (if not NULL):<br>
                             1234567890123456<br>
                             1997-07-16_19:06<br>
                             0123456789012345
     @return # seconds since 00:00:00 UTC, January 1, 1970
  */
  char dat1[18];
  strncpy (dat1,date,18);
  dat1[17] = '\0';
  return hlr_date2time_d (dat1,time_strp);
}

void hlr_dateAdd (char *date,int minutes) {
  /**
     Move date of the form '1997-07-16_00:00'
     forward by 'minutes' minutes. Minutes can be negative, too.
     @param[in] date - in the format '1997-07-16_00:00'
     @param minutes - number of minutes
     @param[out] date - new date
  */
  struct tm time_str;
  hlr_date2time_d (date,&time_str);
  time_str.tm_min += minutes;
  mktime (&time_str);
  sprintf (date,"%04d-%02d-%02d_%02d:%02d",
           time_str.tm_year+1900,time_str.tm_mon+1,
           time_str.tm_mday,time_str.tm_hour,time_str.tm_min);
}

int hlr_dateDiff (char *date1, char *date2) {
  /**
     Compute time difference date1 - date2
     @param[in] date1 - in the form '1997-07-16_00:00'
     @param[in] date2 - in the form '1997-07-16_00:00'
     @return difference in minutes
  */
  struct tm time_str1;
  struct tm time_str2;
  /* buffer and copy one byte more than necessary for
     detecting inputs that are too long */
  char dat1[18];
  char dat2[18];
  strncpy (dat1,date1,18);
  dat1[17] = '\0';
  strncpy (dat2,date2,18);
  dat2[17] = '\0';
  return (hlr_date2time_d (dat1,&time_str1) -
          hlr_date2time_d (dat2,&time_str2)) / 60;
}
