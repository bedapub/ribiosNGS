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
/** @file rofutil.c
    @brief Purpose: file handling utilities (and related stuff).
    Module prefices: hlr_, dio_
*/

#if BIOS_PLATFORM == BIOS_PLATFORM_LINUX && BIOS_BITS_PER_INT == 32
/* this overrides features.h (which in included by stat.h
   make sure you do not #include <sys/stat.h> before #include "rofutil.h" (!)
*/
#define _FILE_OFFSET_BITS 64
#endif

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <fcntl.h>
#include <pwd.h>
#include <errno.h>
#include <dirent.h>
#include <fnmatch.h>

#include "plabla.h"
#include "log.h"
#include "format.h"
#include "hlrmisc.h"

#include "rofutil.h"

void hlr_setCoredumpsize (int size) {
  /**
     Sets the maximum size of a core dump to be written
     @param[in] size - max core dump size in bytes, 0 = unlimited
  */
  struct rlimit rl;
  rl.rlim_cur = size ? size : RLIM_INFINITY;
  setrlimit (RLIMIT_CORE,&rl);
}

#if BIOS_PLATFORM == BIOS_PLATFORM_SOLARIS
static int scandir_BSD (char *dirname,struct dirent ***in_names,char *filter);
#endif

static int gLastLocked = -1; // file descriptor for last file that has been locked

char* hlr_queryPlainFile (char *filename) {
  /**
     Checks if 'filename' is the name of a plain file
     @param[in] filename - the name of the file
     @return NULL if filename is ok, else a diagnostic string
  */
  struct PLABLA_STAT s;

  if (PLABLA_STAT (filename,&s))
    return "file not accessible";
  if ((s.st_mode & S_IFMT) != S_IFREG)
    return "is not a plain file";
  return NULL;
}

char* hlr_queryDir (char *dirname) {
  /**
     Checks if dirname is a directory
     @param[in] dirname - the name of the directory
     @return NULL if dirname is ok, else a diagnostic string
  */
  struct PLABLA_STAT s;
  if (PLABLA_STAT (dirname, &s))
    return "directory not accessible";
  if ((s.st_mode & S_IFMT) != S_IFDIR)
    return "is not a directory";
  return NULL;
}

char *hlr_tail (char *fileNameWithPath) {
  /**
     Produces the same result as the :t (tail) modifier
     available in the UNIX C-shell (csh), fr example<br>
     Examples: ../somepath/file  --> pointer to the beginning of 'file'<br>
               file              --> pointer to the beginning of 'file'
               @param[in] fileNameWithPath -
    @return pointer to the first character of the filename
             without the leading path
  */
  char *slashp = strrchr (fileNameWithPath,'/');
  return slashp ? slashp+1 : fileNameWithPath;
}

char *hlr_getDir (char *fileNameWithPath) {
  /**
     Returns the directory part of a full path name omitting the filename
     --> opposite of hlr_tail(), no analog modifier in csh<br>
     Examples: "/" --> "/"<br>
               "abc" --> ""<br>
               "/abc" --> "/"<br>
               "/abc/" --> "/abc"<br>
               "/abc/def" --> "/abc"<br>
               "abc/def" --> "abc"<br>
               "./abc" --> "."<br>
               "../abc/def" --> "../abc"
     @param[in] fileNameWithPath - full path name to be split, must not be NULL
     @return pointer to directory part without a trailing separator '/'
             except for root directory, memory is read-only to user
  */
  static char buf[MAXPATHLEN+1];
  char *cp = (char *)ILLADR;
  strcpy (buf,fileNameWithPath);
  if ((cp = strrchr (buf,'/')) != NULL)
    if (cp == &buf[0]) // if rightmost '/' is at start, keep it
      *(cp + 1) = '\0';
    else
      *cp = '\0';
  else
    buf[0] = '\0';
  return buf;
}

FILE *hlr_fopenRead (char *filename) {
  /**
     Open a file for read, resp. stdin if filename is '-';
     abort if file not readable
     @param[in] filename -the file to be opened
     @return file pointer - use fclose() when done
  */
  FILE *f = strcmp (filename,"-") ? fopen (filename,"r") : stdin;
  if (f == NULL)
    die ("hlr_fopenRead: %s: %s",strerror (errno),filename);
  return f;
}

FILE *hlr_fopenWrite (char *filename) {
  /**
     Open a file for writing, resp. stdout if filename is '-';
     abort if file not writeable
     @param[in] filename -the file to be opened
     @return file pointer - use fclose() when done
  */
  FILE *f = strcmp (filename,"-") ? fopen (filename,"w") : stdout;
  if (f == NULL)
    die ("hlr_fopenWrite: %s: %s",strerror (errno),filename);
  return f;
}

FILE *hlr_fopenAppend (char *filename) {
  /**
     Open a file for appending, resp. stdout if filename is '-';
     abort if file not writeable
     @param[in] filename -the file to be opened
     @return file pointer -- use fclose() when done
  */
  FILE *f = strcmp (filename,"-") ? fopen (filename,"a") : stdout;
  if (f == NULL)
    die ("hlr_fopenAppend: %s: %s",strerror (errno),filename);
  return f;
}

char *hlr_getUser (void) {
  /**
     Determine name of user of current process by asking
     the operating system
     @return username
  */
  static char *user=NULL;
  if (user == NULL) {
    struct passwd *pp;
    if (! (pp = getpwuid (getuid ()))) {
      die ("hlr_getUser: %s",strerror (errno));
    }
    user = hlr_strdup (pp->pw_name);
  }
  return user;
}

int hlr_isAbsolutePath (char *path) {
  /**
     Determine if supplied path is absolute or relative
     @param[in] path - any path, directory or filename
     @return 1 if path is absolute, 0 if relative
  */
  return path[0]=='/';
}

char *hlr_extension (char *filename) {
  /**
     Find file extension
     @param[in] filename -
     @return pointer to first character of extension in filename, if any,
     else NULL
  */
  return strrchr (hlr_tail (filename),'.');
}

int hlr_fileLock (char *filename) {
  /**
     Acquire lock on file with name 'filename'. Wait, if the lock is
     held by some other process until released.<br>
     Postcondition: another process calling hlr_fileLock(filename) will wait
                    until hlr_fileUnlock(filename) is called or the process
                    holding the lock terminates. hlr_fileLockNB(filename) will
                    see the lock.<br>
                    hlr_fileUnlock(-1) is equivalent to hlr_fileUnlock(lockID
                    just returned)
     @param[in] filename - name of an existing file that is readable
                           to the caller
     @return lockID, this is used as an argument for hlr_fileUnlock()
  */
  /*
    Notes:
    - one process can acquire several locks (one per file); for concurrent
      processes be sure to acquire the locks in the same order or
      deadlock will result
    - be careful with fork(), dup() and the like in this context
    - it is not advisable to lock the same file multiple times within one
      process
    - The last lockID is remembered:
        hlr_fileLock ("myfile");
        hlr_fileUnlock (-1); // unlocks "myfile"
        hlr_fileLock ("myfile1");
        hlr_fileLockNB ("myfile2");
        hlr_fileUnlock (-1); // unlocks "myfile2" !
        hlr_fileUnlock (-1); // die()
    - The flock mechanism does not work for NFS mounted media and produces
      the error 'No locks available' if tried. fcntl could be used instead
      of flock to avoid this problem, if needed.
    Implementation note:
    the 'lockid' returned is acutually a file handle and can be used
    with read() and write() -- but at your own risk!
  */
  gLastLocked = open (filename,PLABLA_FLOCK_OPENFFLAG,0);
  if (gLastLocked == -1)
    die ("hlr_fileLock: cannot open: %s: %s",strerror (errno),filename);
  if (PLABLA_FLOCK (gLastLocked) == -1)
    die ("hlr_fileLock: flock failed: %s",strerror (errno));
  return gLastLocked;
}

int hlr_fileLockNB (char *filename) {
  /**
     Same as hlr_fileLock(), but NB (non-blocking).<br>
     Postcondition: like for hlr_fileLock() if lock was obtained;
                    If lock was not obtained, hlr_fileUnlock(-1) is illegal.<br>
     Notes: see hlr_fileLock()
     @param[in] filename - like for hlr_fileLock()
     @return lockID (>=0) if lock could be obtained;
                    -1 if file is already locked by another process
  */
  int status;

  gLastLocked = open (filename,PLABLA_FLOCK_OPENFFLAG,0);
  if (gLastLocked == -1)
    die ("hlr_fileLockNB: cannot open: %s: %s",strerror (errno),filename);
  if ((status = PLABLA_FLOCKNB (gLastLocked)) == -1) {
    if (errno != PLABLA_ISLOCKED)
      die ("hlr_fileLockNB: flock failed : %s",strerror (errno));
    close (gLastLocked);
    gLastLocked = -1;
  }
  else {
    hlr_assert (status==0,"hlr_fileLockNB");
  }
  return gLastLocked;
}

void hlr_fileUnlock (int lockID) {
  /**
     Release lock identifed by lockID. If some other
     process was waiting on this lock, this other process will
     continue running.<br>
     Precondition: successful call to hlr_fileLock() or
                   hlr_fileLockNB() not having returned -1
     @param[in] lockID - ID of lock obtained from hlr_fileLock() or
                         hlr_fileLockNB() or -1 to release the last lock
                         obtained (see example in hlr_fileLock())
  */
  if (lockID == -1) {
    if (gLastLocked == -1)
      die ("hlr_fileUnlock: the last locked file is not defined");
    lockID=gLastLocked;
  }
  if (PLABLA_FUNLOCK (lockID) == -1)
    die ("hlr_fileUnlock: unlock failed: %s",strerror (errno));
  close (lockID);
  gLastLocked = -1;
}

char *hlr_fileRead (char *filename,int isText,int *len) {
  /**
     Read file input main memory as fast as possible.<br>
     Memory block has size '*len' if isText==0, else one byte more (for the
     '\0'). NULL if file could not be read.<br>
     Memory returned belongs to caller -- call hlr_free() after use.
     Postcondition: if NULL is returned, warnReport() tells the reason for
                    failure<br>
     Limitations: Only for files up to 2GB size. Cannot be used if the file is
                  simultanously written to while reading.
     @param[in] filename -
     @param[in] isText - if 1 '\0' is appended to return string, else 0.
     @param[in] len - can be NULL, if not interested
     @param[out] *len - if not NULL, contains length of file
     @return pointer to memory block containing file contents.
  */
  size_t size;
  struct stat filestatus;
  FILE *f;
  int fd;
  char *contents;

  if ((f = fopen (filename, "r")) == NULL) {
    static Stringa s = NULL;
    stringCreateOnce (s,100);
    stringPrintf (s,"%s: %s",strerror (errno),filename);
    warnAdd ("hlr_fileRead()",string (s));
    return NULL;
  }
  fd = fileno (f);
  if (fstat (fd,&filestatus)) {
    warnAdd ("hlr_fileRead()","fstat failed");
    fclose (f);
    return NULL;
  }
  if (filestatus.st_size > (size_t)((2<<31)-1)) {
    warnAdd ("hlr_fileRead()","file size is limited to 2GB");
    fclose (f);
    return NULL;
  }
  contents = (char *)hlr_malloc (filestatus.st_size + (isText ? 1 : 0));
  size = fread (contents,1,filestatus.st_size,f);
  fclose (f);
  hlr_assert (size == filestatus.st_size,
              "hlr_fileRead(): size mismatch. Simultanous writer active?");
  if (isText)
    contents[filestatus.st_size] = '\0';
  if (len != NULL)
    *len = filestatus.st_size;
  return contents;
}

int hlr_fileEqual (char *fn1,char *fn2) {
  /**
     Compares two files
     @param[in] fn1 - first file name
     @param[in] fn2 - second file name
     @return 1 if file fn1 and fn2 have identical contents, else 0
  */
  FILE *f1;
  FILE *f2;
  int c1 = 0;
  int c2 = 0;
  int isEqual;
  if ((f1 = fopen (fn1,"rb")) == NULL)
    die ("fileEqual: %s: %s",fn1,strerror (errno));
  if ((f2 = fopen (fn2,"rb")) == NULL)
    die ("fileEqual: %s: %s",fn2,strerror (errno));
  while (c1 == c2 && c1 != EOF) {
    c1 = getc (f1);
    c2 = getc (f2);
  }
  isEqual = (c1 == EOF && c2 == EOF);
  fclose (f1);
  fclose (f2);
  return isEqual;
}

char *hlr_fileRemove (char *fn) {
  /**
     Remove a file or empty directory
     @param[in] fn - name of file or directory to be removed
     @return NULL if ok, else an appropriate error message
             memory pointed to is read-only
  */
  if (remove (fn) != 0)
    return strerror (errno);
  return NULL;
}

char *hlr_fileCopy (char *fn1,char *fn2) {
  /**
     Copy a file<br>
     Note: an existing file fn2 is overwritten when permissions allow it;
           the new file fn2 receives new creation/modification times;
           the referred file will be copied instead of a symbolic link;
           directories cannot be copied this way.<br>
     Note: the current implementation might be ineffcient for very large
           files. this can be optimized if necessary.
     @param[in] fn1 - source filename
     @param[in] fn2 - target filename
     @return NULL if ok, else an appropriate error message
             memory pointed to is read-only
  */
  char *result = NULL;
  static char msg[32];
  FILE *f1 = (FILE *)ILLADR;
  FILE *f2 = (FILE *)ILLADR;
  int byte = 0;

  if ((f1 = fopen (fn1,"rb")) != NULL) {
    if ((f2 = fopen (fn2,"wb")) != NULL) {
      while ((byte = getc (f1)) != EOF)
        putc (byte,f2);
      fclose (f2);
    }
    else {
      sprintf (msg,"file cannot be written");
      result = msg;
    }
    fclose (f1);
  }
  else {
    sprintf (msg,"file cannot be read");
    result = msg;
  }
  return result;
}

char *hlr_fileMove (char *fn1,char *fn2) {
  /**
     Move/rename a file
     @param[in] fn1 - source filename
     @param[in] fn2 - target filename
     @return NULL if ok, else an appropriate error message
             memory pointed to is read-only
  */
  char *msg;

  if ((msg = hlr_fileCopy (fn1, fn2)) == NULL)
    msg = hlr_fileRemove (fn1);
  return msg;
}

char *hlr_mkDir (char *dirname) {
  /**
     Create a directory
     @param[in] dirname - name of directory to be created
     @return NULL if ok, else an appropriate error message
             memory pointed to is read-only
  */
  // default mode for directory creation: 775
  mode_t mode = S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH;
  if (mkdir (dirname, mode) != 0)
    return strerror (errno);
  return NULL;
}

char *hlr_getHomeDir (char *username) {
  /**
     Get the home directory of username
     @param[in] username
     @return home directory path, NULL if username does not exist;
             the memory returned is read-only
             stable until the next call to this function
  */
  struct passwd *pass;
  if (username == NULL)
     die ("hlr_getHomeDir (NULL)");
  pass = getpwnam (username);
  if (pass == NULL)
     return NULL;
  return pass->pw_dir;
}

char *hlr_getCurrentExecutable (void) {
  /**
     Returns location and name of executable file of this process.
     Memory onwned by this routine. Read only to caller of this routine.
  */
  /*
    adapted from http://www.linuxquestions.org/questions/programming-9/get-full-path-of-a-command-in-c-117965/
  */
  /*
    /proc/self is a symbolic link to the process-ID subdir of /proc,
    e.g. /proc/4323 when the pid of the process of this program is 4323.
    Inside /proc/<pid> there is a symbolic link to the executable that is
    running as this <pid>. This symbolic link is called "exe".
    So if we read the path where the symlink /proc/self/exe points to we have
    the full path of the executable.
  */
  int length;
  static char fullpath[MAXPATHLEN];
  length = readlink ("/proc/self/exe",fullpath,sizeof (fullpath));
  // catch some errors:
  if (length < 0)
    die ("getCurrentExecutable: Error resolving symlink /proc/self/exe.");
  if (length >= MAXPATHLEN)
    die ("getCurrentExecutable: Path too long.");
  /* I don't know why, but the string this readlink() function
     returns is appended with a '@'.
  */
  fullpath[length] = '\0'; // Strip '@' off the end.
  return fullpath;
}

int hlr_isRunningAlready (void) {
  /**
     Determines executable file of current process and returns 0,
     if this is the only process executing this file at this time,
     and 1 if there are two or more processes executing this file.
     I.e. for the first process this routine returns 0,
     for a second process started while the first is still executing
     this routine returns 1.<br>
     Make sure you place the call to this routine at some place in your
     code before it does any work that does not tolerate parallel runs.<br>
     Typical use:
     if (hlr_isRunningAlready ()) die ("already running. Abort.");
     @return 0 or 1
  */
  int lockid = hlr_fileLockNB (hlr_getCurrentExecutable ());
  return lockid == -1 ? 1 : 0;
}

/* ------------------------------------------------------------- */

/*
  This variable is used for communication between functions
  hlr_dirScanInit() and dirScanSelect() during execution of
  hlr_dirScanInit() -- this implementation is NOT multi-thread safe!
*/
static char *gDirScanFilter = NULL;

#if BIOS_PLATFORM == BIOS_PLATFORM_IRIX || BIOS_PLATFORM == BIOS_PLATFORM_SOLARIS
static int dirScanSelect (struct dirent *de) {
  return (fnmatch (gDirScanFilter,de->d_name,0) == 0);
}
#endif

#if BIOS_PLATFORM == BIOS_PLATFORM_LINUX
static int dirScanSelect (const struct dirent *de) {
  return (fnmatch (gDirScanFilter,de->d_name,0) == 0);
}
#endif

static void updatePath (DirScanIter ds) {
  if (ds->indexPath != ds->index) {
    ds->indexPath = ds->index;
    if (strEqual (ds->dir,"/"))
      stringPrintf (ds->fullPath,"/%s",ds->file);
    else
      stringPrintf (ds->fullPath,"%s/%s",ds->dir,ds->file);
  }
}

static void updateStat (DirScanIter ds) {
  if (ds->index != ds->indexStat) {
    updatePath (ds);
    ds->indexStat = ds->index;
    if (PLABLA_LSTAT (string (ds->fullPath),&ds->s)) {
      perror (string (ds->fullPath));
      warn ("file %s not accessible",string (ds->fullPath));
    }
  }
}

DirScanIter hlr_dirScanInit (char *dirname,char *filter) {
  /**
     Initialize a scan over files in directory dirname matching expression
     filter.<br>
     Postcondition: hlr_dirScanNext() etc. can be called
     @param[in] dirname - name of directory to be scanned, must not be NULL,
                          must not have a trailing path separator ('/' or '\'
                          depending on platform) except for the root directory
     @param[in] filter - scan only for files matching this pattern, may contain
                         wildcard characters '?' and '*', if NULL all files
     @return an iterator which must be used as an argument to hlr_dirScanNext()
             and other hlr_dirScanXyz() functions or NULL if error
             to release temporary resources call hlr_dirScanBreak()
             if iterator is not read to its end
             (i.e. until hlr_dirScanNext() returns NULL)
  */
  int numEntries = 0;
  struct dirent **namelist = (struct dirent **)ILLADR;
  DirScanIter ds = NULL;

  gDirScanFilter = filter;

#if BIOS_PLATFORM == BIOS_PLATFORM_IRIX || BIOS_PLATFORM == BIOS_PLATFORM_LINUX
  if ((numEntries = scandir (dirname,&namelist,
                             filter ? &dirScanSelect : NULL,
                             &alphasort)) != -1)
#endif
#if BIOS_PLATFORM == BIOS_PLATFORM_SOLARIS
  if ((numEntries = scandir_BSD (dirname,&namelist,
                                 filter)) != -1)
#endif
    {
      ds = (DirScanIter)hlr_malloc (sizeof (struct DirScanIterStruct));
      ds->numEntries = numEntries;
      ds->index = -1;
      ds->namelist = namelist;
      ds->dir = hlr_strdup (dirname);
      ds->file = NULL;
      ds->fullPath = stringCreate (256);
      ds->indexPath = -1;
      ds->indexStat = -1;
    }
  return ds;
}

char *hlr_dirScanNext (DirScanIter ds) {
  /**
     Retrieve next entry from current directory scan; the
     exact order is undefined; if it looks alphabetically this is
     by chance and should not be taken for granted.<br>
     Precondition: successful call to hlr_dirScanInit()
     Postcondition: next call to this function will return next entry;
                    file properties can be queried using other hlr_dirScanXyz()
                    functions;<br>
                    hlr_dirScanBreak() must be called if scan is cancelled
                    before this function returned NULL;<br>
                    no other hlr_dirScanXyz() function must be called for this
                    ds after this function has returned NULL
      @param[in] ds - directory scan iterator received from hlr_directoryScanInit()
     @param[out] ds - updated
     @return name of next file (, directory, link) from scanned directory
             or NULL if no more filenames;<br>
             only valid until this function returns NULL because iterator is
             automatically destroyed after final iteration
 */
  char *result = NULL;

  if (++ds->index < ds->numEntries)
    result = ds->file = ds->namelist[ds->index]->d_name;
  else
    hlr_dirScanDestroy_func (ds);
  return result;
}

/*
  call this macro if a directory scan is cancelled before hlr_dirScanNext()
  has returned NULL to release temporary resources

  #define hlr_dirScanBreak(this1) (hlr_dirScanDestroy_func(this1), this1=NULL)
*/

void hlr_dirScanDestroy_func (DirScanIter ds) {
  /**
     Destroy directory scan iterator and free resources
     Precondition: successful call to hlr_dirScanInit()
     Postcondition: ds is invalidated, do not call any hlr_dirScanXyz()
                    function any more for this ds
     Note: do not call this function explicitly - use hlr_dirScanBreak() if the
           scan is cancelled before its end. otherwise this function is called
           automatically when the last item has been read.
     @param[in] ds - directory scan iterator received from hlr_directoryScanInit()
  */
  for (ds->index=0;ds->index<ds->numEntries;ds->index++)
    free (ds->namelist[ds->index]);
#if BIOS_PLATFORM == BIOS_PLATFORM_SOLARIS
  free (ds->namelist);
#endif
  hlr_free (ds->dir);
  ds->file = NULL;
  stringDestroy (ds->fullPath);
  hlr_free (ds);
}

int hlr_dirScanIsDir (DirScanIter ds) {
  /**
     Find out if current file is a directory.<br>
     Precondition: successful call to hlr_dirScanNext()
     @param[in] ds - directory scan iterator received from hlr_directoryScanInit()
     @return 1 if file is directory, 0 else
  */
  updateStat (ds);
  return S_ISDIR (ds->s.st_mode);
}

char *hlr_dirScanFullPathGet (DirScanIter ds) {
  /**
     Retrieve full path for current file.<br>
     Precondition: successful call to hlr_dirScanNext()
     @param[in] ds - directory scan iterator received from hlr_directoryScanInit()
     @return the full path of the last file returned by hlr_dirScanNext()
  */
  updatePath (ds);
  return string (ds->fullPath);
}

intgr8 hlr_dirScanFileSizeGet (DirScanIter ds) {
  /**
     Retrieve file size of current file.<br>
     Precondition: successful call to hlr_dirScanNext()
     @param[in] ds - directory scan iterator received from hlr_directoryScanInit()
     @return the size of the last file returned by hlr_dirScanNext()
  */
  updateStat (ds);
  return ds->s.st_size;
}

int hlr_dirScanLastUpdateGet (DirScanIter ds) {
  /**
     Retrieve last modification date and time (seconds since 01.01.1970) of
     current file.<br>
     Precondition: successful call to hlr_dirScanNext()
     @param[in] ds - directory scan iterator received from hlr_directoryScanInit()
     @return the last modification date and time of the last file returned by
             hlr_dirScanNext()
  */
  updateStat (ds);
  return ds->s.st_mtime;
}

int hlr_dirScanLastAccessGet (DirScanIter ds) {
  /**
     Retrieve last access date and time (seconds since 01.01.1970) of current
     file.<br>
     Precondition: successful call to hlr_dirScanNext()
     @param[in] ds - directory scan iterator received from hlr_directoryScanInit()
     @return the last access date and time of the last file returned by
             hlr_dirScanNext()
   */
  updateStat (ds);
  return ds->s.st_atime;
}

intgr8 hlr_fileSizeGet (char *filename) {
  /**
     Returns the size in bytes of the file given as argument
     @param[in] filename - the complete path to the file
     @return the size of the file
  */
  struct PLABLA_STAT s;

  PLABLA_STAT (filename,&s);
  return s.st_size;
}

int hlr_fileLastUpdateGet (char *fileName) {
  /**
     Returns the last update time (seconds since 1970) of the file
` */
  struct PLABLA_STAT s;

  PLABLA_STAT (fileName,&s);
  return s.st_mtime;
}

char *hlr_getLink (char *filename) {
  /**
     Returns symbolic link info
     @param[in] filename - name of a file, must not be NULL
     @return full path of file or dir pointed to if filename is a symbolic link,
             NULL if filename is not a symbolic link or an error occured
  */
  static char buf[MAXPATHLEN+1];
  if (readlink (filename,buf,MAXPATHLEN+1) == -1)
    return NULL;
  return buf;
}

#if BIOS_PLATFORM == BIOS_PLATFORM_SOLARIS

static struct dirent * hlr_dirScanCloneDirent (struct dirent *in) {
  struct dirent *o;
  if (in == NULL)
    return NULL;
  o = (struct dirent *)malloc (in->d_reclen);
  memcpy (o,in,in->d_reclen);
  return o;
}

static int hlr_dirScan_alphasort (struct dirent **d1,struct dirent **d2) {
  return strcmp ((*d1)->d_name,(*d2)->d_name);
}

int scandir_BSD (char *dirname,struct dirent ***in_names,char *filter) {
  /**
     This is the Sun version of scandir from BSD (which exists on SGI).
  */
  long int loc;
  DIR *dirp;
  int count = 0;
  int x = 0;
  struct dirent **names;
  struct dirent *out;
  dirp = opendir (dirname);
  if (dirp == NULL) {
    return -1;
  }
  loc = telldir (dirp);
  while (out = readdir (dirp)) {
    if (filter == NULL || dirScanSelect (out))
      count++;
  }
  seekdir (dirp,loc);
  names = (struct dirent **)malloc (sizeof (struct dirent) * count);
  while (out = readdir (dirp)) {
    if (filter == NULL || dirScanSelect (out)) {
      if (out != NULL)
        names[x] = hlr_dirScanCloneDirent (out);
      x++;
    }
  }
  qsort (names,x,sizeof (struct dirent *),(ARRAYORDERF) hlr_dirScan_alphasort);
  *in_names = names;
  closedir (dirp);
  return x;
}

#endif

/*----- DirectoryObject functions */

DirectoryObject dio_create (char *path,char *mask) {
  /**
     Creates a directory object.<br>
     Postcondition: use dio_destroy() after use
     @param[in] path - a directory name
     @param[in] mask - filter for file names to import; wildcards
                       allowed ( [], ?, * );
                       NULL means 'all files except for the
                       ones whose name starts with a dot.
     @return new directory object
  */
  DirScanIter ds;
  char *info;
  Stringa hold;
  int i;
  DirectoryObject this1;
  this1 = (DirectoryObject)malloc (sizeof (struct _dirInfo));
  this1->items = textCreate (10);
  this1->root = stringCreate (10);
  this1->place = -1;
  this1->iterator = NULL;
  this1->files = arrayCreate (10,char *);
  this1->dirs = arrayCreate (10,char *);
  hold = stringCreate (20);
  i = strlen(path) - 1;
  if (path[i] == '/')
    path[i] = '\0';
  stringCpy (this1->root,path);
  ds = hlr_dirScanInit (path,mask);
  if (ds != NULL) {
    while ((info = hlr_dirScanNext (ds))) {
      if (mask != NULL && info[0] == '.')
        continue;
      stringPrintf (hold,"%s/%s",string (this1->root),info);
      i = arrayMax (this1->items);
      textAdd (this1->items,info);
      if (hlr_queryDir (string (hold)) != NULL)
        array (this1->files,arrayMax (this1->files),char *) =
          array (this1->items,i,char *);
      else
        array (this1->dirs,arrayMax (this1->dirs),char *) =
          array (this1->items,i,char *);
    }
  }
  stringDestroy (hold);
  return this1;
}

void dio_iterInit (DirectoryObject this1,int iter_type) {
  /**
     Starts an iteration voer the directory
     @param[in] this1 - created by dio_create()
     @param[in] iter_type - DIO_FILES_ONLY / DIO_DIRECTORIES_ONLY /
                            DIO_FILES_AND_DIRECTORIES
  */
  if (iter_type == DIO_FILES_ONLY)
    this1->iterator = this1->files;
  else if (iter_type == DIO_DIRECTORIES_ONLY)
    this1->iterator = this1->dirs;
  else if (iter_type == DIO_FILES_AND_DIRECTORIES)
    this1->iterator = this1->items;
  this1->place = 0;
}

char *dio_getNext (DirectoryObject this1) {
  /**
     Precondition: dio_iterInit(this1) or dio_getNext(this1)<br>
     Postcondition: next call to this function will return next
                    file name from directory
     @return file name including path; memory managed by this
             routine; read only to user of this routine
  */
  if (this1->place == arrayMax (this1->iterator) || this1->place == -1)
    return NULL;
  else
    return array (this1->iterator,this1->place++,char *);
}

void dio_destroy_func (DirectoryObject this1) {
  /**
     Do not use this function in your program, use dio_destroy()
  */
  if (this1 == NULL)
    return;
  arrayDestroy (this1->dirs);
  arrayDestroy (this1->files);
  stringDestroy (this1->root);
  textDestroy (this1->items);
  free (this1);
}

/* ----------------------- begin module hlr_ipcf ---------------------- */
/*
  Purpose: transport a string from one process to another via temporary file
           (ipcf = inter process communication via file)
  Usage: in first process:
           id = hlr_ipcfSet ("hello!");
         then "somehow" (e.g. as cmd line argument), pass id to second process.
         in second process:
           char *s = hlr_ipcfGetOnce (id);
  Restrictions: one process can only pass one string. The string can
                be arbitrarily long, but cannot contain '\0'.
*/

static char *gIpcFileNameTemplate = "/tmp/hlr_ipcf_%d.tmp";
static int gIpcfCnt = 0;

int hlr_ipcfSet (char *s) {
  /**
     Set the string to be communicated.<br>
     Postcondition: hlr_ipcfGetOnce(id) will return 's', even if
                    call happens in a later process
     @return persistent id assigned to input 's'
  */
  Stringa fn = stringCreate (40);
  FILE *f;
  int id = (getpid () * (1 << 8)) + ++gIpcfCnt;
  stringPrintf (fn,gIpcFileNameTemplate,id);
  if ((f = fopen (string (fn),"wb")) != NULL) {
    fputs (s,f);
    fclose (f);
  }
  else
    die ("hlr_ipcfSet: cannot open file %s",string (fn));
  stringDestroy (fn);
  return id;
}

char *hlr_ipcfGetOnce (int id) {
  /**
     Receive the string set by hlr_ipcfSet() and free the communication
     resource.<br>
     Postcondition: next call to hlr_ipcfGetOnce() will return ""
                    (emtpy string).
     @returns: message - memory managed by this routine; user may read & write,
               but not free() or realloc();
               if id==0 or there was no hlr_ipcfSet() or some problem occured,
               an empty string is returned.
  */
  Stringa fn = stringCreate (40);
  FILE *f;
  char *problem;
  int byte;
  static Stringa msg = NULL;
  stringCreateClear (msg,40);

  if (id == 0)
    return string (msg);
  stringPrintf (fn,gIpcFileNameTemplate,id);
  if ((f = fopen (string (fn),"rb")) != NULL) {
    arrayClear (msg); // not a Stringa anymore
    while ((byte = getc (f)) != EOF)
      array (msg,arrayMax (msg),char) = byte;
    array (msg,arrayMax (msg),char) = '\0'; // convert back into a Stringa
    fclose (f);
    if ((problem = hlr_fileRemove (string (fn))) != NULL)
      stringInsert (msg,0,problem);
  }
  // if there was no hlr_ipcfSet() or some other problem, return an empty string
  stringDestroy (fn);
  return string (msg);
}
/* ----------------------- end module hlr_ipcf ---------------------- */
