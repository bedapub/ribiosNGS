ORACLE.BIA.USER <- "biread"
ORACLE.BIA.PWD <- "biread"
ORACLE.BASE <- "/opt/oracle"
ORACLE.HOME <- "/opt/oracle/client/10/run_1"
## ORACLE.LIB <- ":/opt/oracle/client/10/run_1/lib"

.onLoad <- function(libname, pkgname) {
  assign("ORA", Oracle(), pos=sys.frame())
  obase <- Sys.getenv("ORACLE_BASE")
  ohome <- Sys.getenv("ORACLE_HOME")
  if(identical(obase, "") || identical(obase, ".") || !"client" %in% dir(obase)) {
    Sys.setenv("ORACLE_BASE"=ORACLE.BASE)
  }
  if(identical(ohome, "") || identical(ohome, ".") || !"bin" %in% dir(ohome)) {
    Sys.setenv("ORACLE_HOME"=ORACLE.HOME)
  }
##  new.path <- paste(Sys.getenv("LD_LIBRARY_PATH"), ORACLE.LIB, sep=":");
##  Sys.setenv("LD_LIBRARY_PATH"=new.path)
}

newcon <- function() {
  dbConnect(ORA, user=ORACLE.BIA.USER, password=ORACLE.BIA.PWD, db="bia")
}
