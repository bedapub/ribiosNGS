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
appendLoggers <- function(con.list) {
  loggers <- getLoggers()
  loggers.desc <- sapply(loggers, function(x) summary(x)$description)
  con.descs <- sapply(con.list, function(x) summary(x)$description)
  new.loggers <- !con.descs %in% loggers.desc & !duplicated(con.descs)
  if(any(new.loggers)) {
    loggers <- append(loggers, con.list[new.loggers])
    setLoggers(loggers)
  }
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

registerLog <- function(..., append=FALSE) {
  x <- list(...)
  if(length(x)==0 || (length(x)==1 && (is.null(x[[1]]) || is.na(x[[1]])))) {
    setLoggers(NULL);
    return(invisible(NULL));
  }
  cons <- lapply(x, function(xx) {
    if(is(xx, "connection")) {
      return(xx)
    } else  if(is.character(xx)) {
      if(xx=="-") {
        return(stdout());
      } else if(file.exists(xx) && append) {
        logcon <- file(xx, "a")
      } else {
        logcon <- file(xx, "w")
      }
      return(logcon)
    }
    stop("Input parameters must be either connection or file names.")
  })
  appendLoggers(cons)
  
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

writeLog <- function(fmt, ..., con=stdout(), level=0) {
  format <- paste("[%s] ",
                  paste(rep(" ", level), collapse=""),
                  "%s", sep="")
  rlist <- list(...)
  if(length(rlist)==0) {
    txt <- fmt
  } else if (length(rlist)==1 && is.null(rlist[[1]])) {  ## in case the first value is NULL
    txt <- sprintf(fmt, "NULL")
  } else {
    txt <- sprintf(fmt, ...)
  }
  text <- sprintf(format,
                  format(Sys.time(),"%y-%m-%d %X"),
                  txt)
  writeLines(text, con=con)
}

doLog <- function(fmt, ..., level=0) {
  loggers <- getLoggers()
  if(!is.null(loggers))
    for(i in seq(along=loggers))
      writeLog(fmt, ..., con=loggers[[i]], level=level)
}
