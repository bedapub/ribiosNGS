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
/** @file rofutil.h
    @brief Purpose: file handling utilities (and related stuff).
    Module prefices: hlr_, dio_
*/
#ifndef ROFUTIL_H
#define ROFUTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/stat.h>
#include "plabla.h"
#include "format.h"

extern void hlr_setCoredumpsize (int size);

extern char* hlr_queryPlainFile (char *filename);
extern char* hlr_queryDir (char *dirname);
extern char *hlr_tail (char *fileNameWithPath);
extern char *hlr_getDir (char *fileNameWithPath);
extern FILE *hlr_fopenRead (char *filename);
extern FILE *hlr_fopenWrite (char *filename);
extern FILE *hlr_fopenAppend (char *filename);
extern char *hlr_getUser (void);
extern int hlr_isAbsolutePath (char *path);
extern char *hlr_extension (char *filename);
extern int hlr_fileLock (char *filename);
extern int hlr_fileLockNB (char *filename);
extern void hlr_fileUnlock (int lockID);

extern char *hlr_fileRead (char *filename,int isText,int *len);
extern int hlr_fileEqual (char *fn1,char *fn2);
extern char *hlr_fileRemove (char *fn);
extern char *hlr_fileCopy (char *fn1,char *fn2);
extern char *hlr_fileMove (char *fn1,char *fn2);
extern char *hlr_mkDir (char *dirname);
extern char *hlr_getHomeDir (char *username);

extern char *hlr_getCurrentExecutable (void);
extern int hlr_isRunningAlready (void);

/* ------ do not refer to anything inside these structs from your programs */

/// the DirScanIter object
typedef struct DirScanIterStruct {
  int numEntries; //!< number of entries in the scan
  int index; //!< ???
  struct dirent **namelist; //!< ???
  char *dir; //!< ???
  char *file; //!< ???
  int indexPath; //!< ???
  Stringa fullPath; //!< ???
  int indexStat; //!< ???
  struct PLABLA_STAT s; //!< ???
}*DirScanIter;

/// the DirectoryObject
typedef struct _dirInfo {
  Texta items; //!< files and directories in the directory
  Array dirs; //!< char *; indices of directories
  Array files; //!< char *; indices of files
  Array iterator; //!< stores type of iteration; points to dirs, files or items
  Stringa root; //!< root path
  int place; //!< place holder for last access
}*DirectoryObject;

extern DirScanIter hlr_dirScanInit (char *dirname,char *filter);
extern char *hlr_dirScanNext (DirScanIter ds);
extern void hlr_dirScanDestroy_func (DirScanIter ds);
/// do not call hlr_dirScanDestroy_func, instead call the following macro
#define hlr_dirScanBreak(this1) (hlr_dirScanDestroy_func(this1),this1=NULL)
extern int hlr_dirScanIsDir (DirScanIter ds);
extern char *hlr_dirScanFullPathGet (DirScanIter ds);
extern intgr8 hlr_dirScanFileSizeGet (DirScanIter ds);
extern int hlr_dirScanLastUpdateGet (DirScanIter ds);
extern int hlr_dirScanLastAccessGet (DirScanIter ds);

extern intgr8 hlr_fileSizeGet (char *filename);
extern int hlr_fileLastUpdateGet (char *fileName);
extern char *hlr_getLink (char *filename);

/// for dio_iterInit()
#define DIO_FILES_ONLY 1
/// for dio_iterInit()
#define DIO_DIRECTORIES_ONLY 2
/// for dio_iterInit()
#define DIO_FILES_AND_DIRECTORIES 3

extern DirectoryObject dio_create (char *path,char *mask);
extern void dio_iterInit (DirectoryObject this1,int iter_type);
extern char * dio_getNext (DirectoryObject this1);

extern void dio_destroy_func (DirectoryObject this1);
/// do not call dio_destroy_func(), instead call the following macro
#define dio_destroy(this1) (dio_destroy_func(this1),this1=NULL)

extern int hlr_ipcfSet (char *s);
extern char *hlr_ipcfGetOnce (int id);

#ifdef __cplusplus
}
#endif

#endif
