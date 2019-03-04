#include "ribios_utils.h"

#include <R_ext/Rdynload.h> /* DllInfo is needed to register the routines*/
#include <R_ext/Visibility.h>

#include "r_trim.h"
#include "r_mmatch.h"
#include "r_endec.h"

static R_CallMethodDef callMethods[] = {
  {"pwdecode", (DL_FUNC) &pwdecode, 1},
  {"mmatch", (DL_FUNC) &mmatch, 3},
  {"trim", (DL_FUNC) &trim, 3},
  {NULL, NULL, 0}
};

void R_init_ribiosUtils(DllInfo *info) {
  R_registerRoutines(info, NULL /*CMethods*/, callMethods, NULL, NULL);
  /* the line below says that the DLL is not to be searched
   * for entry points specified by character strings so
   * .C etc calls will only find registered symbols
   */  
  R_useDynamicSymbols(info, FALSE); 
  /* R_forceSymbols call only allows .C etc calls which 
   * specify entry points by R objects such as C_routineName
   * (and not by character strings)
   */ 
  R_forceSymbols(info, TRUE);
}
