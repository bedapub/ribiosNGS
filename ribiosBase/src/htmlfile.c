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
/** @file htmlfile.c
    @brief Handle files coming from an HTML form.
    Module prefix htmlform_
*/
#include <sys/types.h>
#include <unistd.h>
#include "biosdefs.h"
#include "log.h"
#include "format.h"
#include "rofutil.h"
#include "htmlfile.h"

char *htmlfile_msExcelUpload (char *outputFileName,
                              Array value,
                              int workSheetNum,
                              Stringa fileName,
                              Stringa contentType) {
  /**
     Take data from an HTML FORM input and deliver it in
     tab-and-newline-delimited form. Input can be a tab-delimited file or a
     native MS-Excel file.
     Dependency: command line program $BIOINFOBIN/excel2tab must be installed
                 (not part of BIOS; see /SOFT/bi/apps/excel2tab)
     @param[in] outputFileName - if NULL, deliver result in buffer returned and
                                 do not leave any file around; if not NULL,
                                 write result into the file designated by
                                 'outputFileName and return NULL.<br>
     @param[in] value - Array of char (might be binary, typically from
                        cgiMpNext()
     @param[in] workSheetNum - only relevant for Excel workbooks (starts at 0)
     @param[in] fileName - typically from cgiMpNext(), must not be empty
     @param[in] contentType - typically from cgiMpNext()
     @return NULL or buffer with contents; read/write OK, but no realloc
             or free - memory managed by this routine; stable until next call.
  */
  /*
    implementation notes:
    cases:
    outputFileName="xxx", input="f.txt"
    --> copy value to xxx, return NULL
    outputFilename="xxx", input="f.xls"
    --> copy value to tmp file t, excel2tab t xxx, remove t, return NULL
    outputFileName=NULL, input="f.xls"
    --> copy value to tmp file t, excel2tab t xxx, read t into buffer,
        remove t, return buffer
    outputFileName=NULL, input="f.txt"
    --> copy to value to buffer and return it
  */
  int inputIsMSExcel; // 1=excel, 0=ascii text file
  int hasExcelSignature; // text file stored as file with file extension *.xls
  static Stringa tmpFileName = NULL;
  static Stringa tmpFileName2;
  static Stringa cmd = NULL;
  int i;
  int c;
  FILE *fp;
  static Stringa output = NULL;

  stringCreateClear (output,5000);
  stringCreateOnce (cmd,200);
  if (tmpFileName == NULL) {
    tmpFileName = stringCreate (50);
    tmpFileName2 = stringCreate (50);
    stringPrintf (tmpFileName,"/tmp/cgiHandleExcelField.tmp.in.%d",
                  getpid ());
    stringPrintf (tmpFileName2,"/tmp/cgiHandleExcelField.tmp.out.%d",
                  getpid ());
  }
  if (stringLen (fileName) == 0)
    die ("htmlfile_msExcelUpload: fileName empty");
  if (strEqual (string (contentType),"application/octet-stream"))
    die ("<br>Sorry, could not determine type of your input file.<BR>"
         "Possible reasons and fixes:<br>"
         "- the file is a text file, but does not end in .txt. Fix: rename file to end in .txt and re-try<br>"
         "- the file is currently open in MS-Excel or in a word processor. Fix: close the file and re-try");
  /* check if input file has Excel signature. If not and if content type is
     ms-excel, file is considered to be a tab delimited file having file
     extension ".xls" */
  hasExcelSignature = !memcmp (string (value),
                               "\xd0\xcf\x11\xe0\xa1\xb1\x1a\xe1",8);
  if (hasExcelSignature && strEqual (string (contentType),
                                     "application/vnd.ms-excel"))
    inputIsMSExcel = 1;
  else if (strEqual (string (contentType),"") || !hasExcelSignature) {
    inputIsMSExcel = 0;
    // add '\0' to value if not done yet, i.e. if contentType != "text/plain" or contentType != ""
    if (!isBlankStr (string (contentType)))
      array (value,arrayMax (value),char) = '\0';
    if (arrayMax (value) == 0 || arru (value,arrayMax (value)-1,char) != '\0')
      die ("htmlfile_msExcelUpload: unexpected: unterminated text input");
  }
  else
    die ("Could not determine type of your input file.<BR>"
         "If you have it currently open in MS-Excel or in a word processor, please close and re-try.<br>Error: %s",
         string (contentType));
  if (hlr_queryPlainFile (BIOINFOBIN "/excel2tab"))
    die ("BIOS is not fully installed on this server computer,<br>"
         "Solutions: 1) install excel2tab OR 2) use tab-delimited file only<br>"
         "Upload aborted.");
  if (outputFileName != NULL && !inputIsMSExcel) {
    fp = hlr_fopenWrite (outputFileName);
    fputs (string (value),fp);
    fclose (fp);
    return NULL;
  }
  else if (outputFileName != NULL && inputIsMSExcel) {
    fp = hlr_fopenWrite (string (tmpFileName));
    for (i=0;i<arrayMax (value);i++)
      putc (arru (value,i,char),fp);
    fclose (fp);
    stringPrintf (cmd,BIOINFOBIN "/excel2tab '%s' '%s' %d",
                  string (tmpFileName),outputFileName,workSheetNum);
    hlr_system (string (cmd),0);
    hlr_fileRemove (string (tmpFileName));
    return NULL;
  }
  else if (outputFileName == NULL && inputIsMSExcel) {
    fp = hlr_fopenWrite (string (tmpFileName));
    for (i=0;i<arrayMax (value);i++)
      putc (arru (value,i,char),fp);
    fclose (fp);
    stringPrintf (cmd,BIOINFOBIN "/excel2tab '%s' '%s' %d",
                  string (tmpFileName),string (tmpFileName2),workSheetNum);
    hlr_system (string (cmd),1);
    hlr_fileRemove (string (tmpFileName));
    fp = hlr_fopenRead (string (tmpFileName2));
    stringClear (output);
    while ((c = fgetc (fp)) != EOF)
      stringCatChar (output,c);
    hlr_fileRemove (string (tmpFileName2));
    return string (output);
  }
  else if (outputFileName == NULL && !inputIsMSExcel) {
    stringCpy (output,string (value));
    return string (output);
  }
  else
    die ("");
  return NULL;
}
