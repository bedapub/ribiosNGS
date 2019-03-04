## logging facility:
## (1) Script programmer can register one or more loggers by registerLog(): each accepts a file name or a connection
## (2) To print a log, script programmer runs doLog: log information will be printed in all registered connection
## (3) When the script quites, all open loggers are closed automatically. No action is required.

## internal funcs

RIBIOS_LOGGERS <- NULL

#' @importFrom utils getFromNamespace
getLoggers <- function() {
  return(getFromNamespace("RIBIOS_LOGGERS", ns="ribiosUtils"))
}

#' @importFrom utils assignInNamespace
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
#' @export clearLog
clearLog <- function() {
  assignInNamespace("RIBIOS_LOGGERS", NULL, ns="ribiosUtils")
}

#' @export flushLog
flushLog <- function() {
  loggers <- getLoggers()
  if(!is.null(loggers))
    for(i in seq(along=loggers))
      flush(loggers[[i]])
}

#' The functions \code{registerLog} and \code{doLog} provide a simple mechanism
#' to handle loggings (printing text messages to files or other types of
#' connections) in R.
#' 
#' Users can register arbitrary numbers of loggers with \code{registerLog}, and
#' the functions take care of low-level details such as openning and closing
#' the connections.
#' 
#' Input parameters can be either character strings or connections (such as the
#' objects returned by \code{stdout()} or \code{pipe()}.
#' 
#' If a character string is registered as a logger, it is assumed as a file
#' name (user must make sure that it is writable/appendable). In case the file
#' exists, new logging messages will be \emph{appended}; otherwise if the file
#' does not exists, it will be created and the logging messages will be
#' \code{written} to the file.
#' 
#' A special case is the parameter value \code{"-"}: it will be interpreted as
#' standard output.
#' 
#' if a connection is registered as a logger, it must be writable in order to
#' write the logging messages.
#' 
#' Each parameter will be converted to a \code{connection} object, which will
#' be \code{closed} (when applicable) automatically before R quits.
#' 
#' If the parameter is missing (or set to \code{NA} or \code{NULL}), no logging
#' will take place.
#' 
#' @aliases registerLog flushLog clearLog
#' @param \dots Arbitrary numbers of file names (character strings) or
#' connection objects (see example).
#' @param append Logical, log will be appended to the existing file but not
#' overwriting. Only valid for files but not for connections such as standard
#' output.
#' @return No value returned: its side effect is used.
#' @note Currently, the loggers are stored in a variable in the namespace of
#' ribiosUtils named \code{RIBIOS_LOGGERS}. This is only for internal use of
#' the package and may change any time, therefore users are not advised to
#' manipulate this variable directly.
#' 
#' To clear the registered loggers, use \code{clearLog}.To flush the registered
#' loggers, use \code{flushLog}. Usually it is not necessary to use
#' \code{flushLog} in R scripts, since by program exit the active R session
#' will automatically flush and close the connections (in addition, frequent
#' flushing may decrease the program's efficiency). However, if used in
#' interactive sessions, sometimes \code{flushLog} is needed to force R write
#' all log files to all connections that are registered.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{doLog} writes messages iteratively to each connection
#' registered by \code{registerLog}.
#' @examples
#' 
#' logfile1 <- tempfile()
#' logfile2 <- tempfile()
#' logcon3 <- stdout()
#' registerLog("/dev/null")
#' registerLog(logfile1)
#' registerLog(logfile2)
#' registerLog(logcon3)
#' 
#' doLog("Start logging")
#' doLog("Do something...")
#' doLog("End logging")
#' 
#' flushLog() ## usually not needed, see notes
#' 
#' txt1 <- readLines(logfile1)
#' txt2 <- readLines(logfile2)
#' 
#' cat(txt1)
#' cat(txt2)
#' 
#' clearLog()
#' 
#' registerLog(logfile1, logfile2, logcon3)
#' 
#' doLog("Start logging - round 2")
#' doLog("Do something again ...")
#' doLog("End logging - for good")
#' 
#' flushLog() ## usually not needed, see notes
#' 
#' txt1 <- readLines(logfile1)
#' txt2 <- readLines(logfile2)
#' 
#' cat(txt1)
#' cat(txt2)
#' 
#' @importFrom methods is
#' @export registerLog
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

#' Write text as log to a connection
#' 
#' The function \code{writeLog} can be used to log outputs and/or running
#' status of scripts to \emph{one connection}. To use it one does \emph{not}
#' need to run \code{registerLog} first.
#' 
#' In contrast, \code{doLog} can be used to log on multiple connections that
#' are registered by \code{registerLog}. Therefore, to register logger(s) with
#' \code{registerLog} is a prerequisite of calling \code{doLog}. Internally
#' \code{doLog} calls \code{writeLog} sequentially to make multiple-connection
#' logging.
#' 
#' @aliases writeLog doLog
#' @param fmt Format string to passed on to sprintf
#' @param \dots Parameters passed on to sprintf
#' @param con A connection, for instance a file (or its name) or
#' \code{stdout()}
#' @param level Logging level: each higher level will add one extra space
#' before the message. See examples
#' @return Side effect is used.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{registerLog} to register more than one loggers so that
#' \code{doLog} can write to them sequentially.
#' @examples
#' 
#' writeLog("This is the start of a log")
#' writeLog("Message 1", level=1)
#' writeLog("Message 1.1", level=2)
#' writeLog("Message 1.2", level=2)
#' writeLog("Message 2", level=1)
#' writeLog("Message 3", level=1)
#' writeLog("Message 3 (special)", level=4)
#' writeLog("End of the log");
#' 
#' ## log with format
#' writeLog("This is Message %d", 1)
#' writeLog("Square of 2 is %2.2f", sqrt(2))
#' 
#' ## NA is handled automatically
#' writeLog("This is a not available value: %s", NA, level=1)
#' writeLog("This is a NULL value: %s", NULL, level=1)
#' 
#' @export writeLog
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


#' @export doLog
doLog <- function(fmt, ..., level=0) {
  loggers <- getLoggers()
  if(!is.null(loggers))
    for(i in seq(along=loggers))
      writeLog(fmt, ..., con=loggers[[i]], level=level)
}
