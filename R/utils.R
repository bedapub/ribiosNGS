ORACLE.USER <- "biread"
ORACLE.PWD <- "biread"

.onLoad <- function(libname, pkgname) {
  assign("ORA", Oracle(), pos=sys.frame())
}

newcon <- function() {
  dbConnect(ORA, user=ORACLE.USER, password=ORACLE.PWD,
                   db="bia")
}
