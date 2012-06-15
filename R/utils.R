## bi@bia
ORACLE.BIA.USER <- "bi"
ORACLE.BIA.PWD <- "bi"
## bi2@bia
ORACLE.BIA2.USER <- "bi2"
ORACLE.BIA2.PWD <- "wolf"
## biread@bia
ORACLE.BIARO.USER <- "biread"
ORACLE.BIARO.PWD <- "biread"
## red
ORACLE.RED.USER <- "red"
ORACLE.RED.PWD <- "red"
## genome
ORACLE.BIN.USER <- "genome"
ORACLE.BIN.PWD <- "genome"
ORACLE.BASE <- "/opt/oracle"
ORACLE.HOME <- "/opt/oracle/client/10/run_1"
## ORACLE.LIB <- ":/opt/oracle/client/10/run_1/lib"

## maximum vector length in the IN syntax
ORACLE.IN.NMAX <- 1000L

.onLoad <- function(libname, pkgname) {
  obase <- Sys.getenv("ORACLE_BASE")
  ohome <- Sys.getenv("ORACLE_HOME")
  if(identical(obase, "") || identical(obase, ".") || !"client" %in% dir(obase)) {
    Sys.setenv("ORACLE_BASE"=ORACLE.BASE)
  }
  if(identical(ohome, "") || identical(ohome, ".") || !"bin" %in% dir(ohome)) {
    Sys.setenv("ORACLE_HOME"=ORACLE.HOME)
  }
  assign("ORA", Oracle(), pos=sys.frame())
##  new.path <- paste(Sys.getenv("LD_LIBRARY_PATH"), ORACLE.LIB, sep=":");
##  Sys.setenv("LD_LIBRARY_PATH"=new.path)
}

## shortcuts for common connections
newconBIA <- function() dbConnect(ORA, user=ORACLE.BIA.USER, password=ORACLE.BIA.PWD, db="bia")
newconBIA2 <- function() dbConnect(ORA, user=ORACLE.BIA2.USER, password=ORACLE.BIA2.PWD, db="bia")
newconBIARO <- function() dbConnect(ORA, user=ORACLE.BIARO.USER, password=ORACLE.BIARO.PWD, db="bia")
newconBIN <- function() dbConnect(ORA, user=ORACLE.BIN.USER, password=ORACLE.BIN.PWD, db="bin")
newconRED <- function() dbConnect(ORA, user=ORACLE.RED.USER, password=ORACLE.RED.PWD, db="bia")

## format IN syntax
formatIn <- function(x)
  paste("(",paste("'", x, "'", sep="", collapse=","),")", sep="")

## obsolete (depcreated) functions
biosCurrentGeneSymbol <- function(...) {
  .Deprecated("gtiChipAnnotation",
              package="ribiosAnnotation")
  gtiChipAnnotation(...)
}

raceChipAnnotation <- function(...) {
  .Deprecated("gtiChipAnnotation",
              package="ribiosAnnotation")
  gtiChipAnnotation(...)
}
