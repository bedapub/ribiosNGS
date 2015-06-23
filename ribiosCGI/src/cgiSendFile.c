#include "rofutil.h"

#include "ribios_cgi.h"
#include "cgi.h"
#include "cgiSendFile.h"

SEXP r_cgiSendFile(SEXP file, SEXP header) {
  if(header != R_NilValue)
    r_cgiHeader(header);

  char *cFile=cStr(file);
  static char* line=NULL;
  static int len=0;

  line=hlr_fileRead(cFile, 0, &len);
  fwrite(line, 1, len, stdout);
  fflush(stdout);
  return R_NilValue;
}
