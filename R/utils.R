ORACLE.USER <- "biread"
ORACLE.PWD <- "biread"

.onLoad <- function(libname, pkgname) {
  assign("ORA", Oracle(), pos=sys.frame())
  obase <- Sys.getenv("ORACLE_BASE")
  ohome <- Sys.getenv("ORACLE_HOME")
  if(identical(obase, "") || identical(obase, ".") || !"client" %in% dir(obase)) {
    Sys.setenv("ORACLE_BASE"="/opt/oracle")
  }
  if(identical(ohome, "") || identical(ohome, ".") || !"bin" %in% dir(ohome)) {
    Sys.setenv("ORACLE_HOME"="/opt/oracle/client/10/run_1")
  }
}

newcon <- function() {
  dbConnect(ORA, user=ORACLE.USER, password=ORACLE.PWD,
                   db="bia")
}
