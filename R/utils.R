ORACLE.BIA.USER <- "bi"
ORACLE.BIA.PWD <- "bi"
ORACLE.BIN.USER <- "genome"
ORACLE.BIN.PWD <- "genome"
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

newconBIA <- function() {
  dbConnect(ORA, user=ORACLE.BIA.USER, password=ORACLE.BIA.PWD, db="bia")
}
newconBIN <- function() {
  dbConnect(ORA, user=ORACLE.BIN.USER, password=ORACLE.BIN.PWD, db="bin")
}
