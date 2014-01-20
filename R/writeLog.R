## logging facility:
## (1) Script programmer can register one or more loggers by registerLog(): each accepts a file name or a connection
## (2) To print a log, script programmer runs doLog: log information will be printed in all registered connection
## (3) When the script quites, all open loggers are closed automatically. No action is required.

## internal funcs

RIBIOS_LOGGERS <- NULL

getLoggers <- function() {
  return(getFromNamespace("RIBIOS_LOGGERS", ns="ribiosUtils"))
}
setLoggers <- function(loggers) {
  assignInNamespace("RIBIOS_LOGGERS", loggers, ns="ribiosUtils")
}

## exported funcs
clearLog <- function() {
  assignInNamespace("RIBIOS_LOGGERS", NULL, ns="ribiosUtils")
}

flushLog <- function() {
  loggers <- getLoggers()
  if(!is.null(loggers))
    for(i in seq(along=loggers))
      flush(loggers[[i]])
}

registerLog <- function(...) {
  x <- list(...)
  if(length(x)==0 || is.null(x) || is.na(x)) {
    setLoggers(NULL);
    return();
  }
  cons <- lapply(x, function(xx) {
    if(is(xx, "connection")) {
      return(xx)
    } else  if(is.character(xx)) {
      if(xx=="-") {
        return(stdout());
      } else if(file.exists(xx)) {
        logcon <- file(xx, "a")
      } else {
        logcon <- file(xx, "w")
      }
      return(logcon)
    }
    stop("Input parameters must be either connection or file names.")
  })
  loggers <- getLoggers()
  cons <- append(loggers, cons)
  setLoggers(cons)
  
  ## When the R session ends, RIBIOS_LOGGERS should be closed whenever possible
  myLast <- function(x) {
    loggers <- getLoggers();
    if(is.null(loggers)) return;
    
    for(i in seq(along=loggers)) {
      con <- loggers[[i]]
      if(is(con, "connection") & !is(con, "terminal"))
        close(con)
    }
  }
  assign(".Last", myLast, envir=.GlobalEnv)
  return(invisible(NULL))
}

writeLog <- function(..., con=stdout(), level=0) {
  format <- paste("[%s] ",
                  paste(rep(" ", level), collapse=""),
                  "%s", sep="")
  text <- sprintf(format,
                  format(Sys.time(),"%y-%m-%d %X"),
                  paste(list(...), collapse=" "))      
  writeLines(text, con=con)
}

doLog <- function(..., level=0) {
  loggers <- getLoggers()
  if(!is.null(loggers))
    for(i in seq(along=loggers))
      writeLog(..., con=loggers[[i]], level=level)
}
