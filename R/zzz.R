.onLoad <- function(libname, pkgname){
      require("AnnotationDbi", quietly=TRUE)
      where = asNamespace(pkgname)
      data(list = pkgname, package = pkgname, envir = where)
      packageStartupMessage(AnnotationDbi:::annoStartupMessages(pkgname))
}
