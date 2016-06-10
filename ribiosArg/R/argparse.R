argParse <- function(optargs, reqargs, usage=paste(scriptName(), "-h"), strict=TRUE) {
  if(isDebugging()) {
    message("[DEBUGGIING] The script is running in an interactive session, e.g. debugging mode")
    return(invisible(NULL))
  }
  allComm <- commandArgs(FALSE)
  isFile <- grepl("^--file=", allComm)
  isF <- grepl("^-f", allComm)
  isE <- grepl("^-e", allComm)
  isArgs <- grepl("^--args", allComm)
  isE <- grepl("^-e", allComm)
  if( any(isFile) ) {
    find <- which(isFile)
    allComm[find] <- gsub("^--file=", "", allComm[find])
    allComm <- allComm[-c(1:(find-1))]
  } else if (any(isF)) {
    find <- which(isF)
    allComm <- allComm[-(1:find)]
  } else if (any(isArgs)) {
    find <- which(isArgs)
    allComm <- allComm[-(1:find)]
  } else if (any(isE)) {
    efind <- which(isE)
    allComm <- allComm[-(1:(efind+1))]
  } else {
    stop("This should not happen: no parameters in the form of '-f' or '--f' is detected. Please contact the developer")
  } 
  comm <- allComm[!grepl("^--", allComm)]
  ## the following code was valid till R-3.0.x. 
  ##  if("--args" %in% allComm) {
  ##    indFile <- grep("^--file=", allComm)
  ##    allComm[indFile] <- gsub("^--file=", "", allComm[indFile]) ## script name
  ##    allComm <- allComm[-c(1:(indFile-1))]
  ##    comm <- allComm[!grepl("^--", allComm)]
  ##  } else {
  ##    comm <- allComm[-c(1:2)]
  ##  }
  if(is.null(optargs)) optargs <- ""
  if(is.null(reqargs)) reqargs <- ""
  argc <- as.integer(length(comm))
  usage <- paste(usage,
                 ifelse(grepl("\n$", usage), "", "\n"),
                 "\n[Last call: ",
                 paste(commandArgs(trailingOnly=FALSE), collapse=" "),
                 "]\n",
                 sep="")
  res <- .Call("rarg_parse",
               argc,
               as.character(comm),
               as.character(optargs),
               as.character(reqargs),
               as.character(usage))

  if(res<0)
      qqmsg()
  
  if(strict) {
    if(argc != res)
      qqmsg()
    return(invisible(NULL))
  } else {
    if(argc >= (res+1)) {
      return(invisible(comm[(res+1):argc]))
    } else {
      return(invisible(NULL))
    }
  }
}

argIsInit <- function() .Call("rarg_isInit")

argPresent <- function(opt) {
    if(isDebugging()) {
        message("[DEBUGGIING] The script is running in an interactive session, e.g. debugging mode. FALSE is returned")
        return(FALSE)
    }
  .Call("rarg_present", opt)
}

#' Parse an argument with the given position
#'
#' Get the value of an named argument with the given position
#'
#' @param opt name of the argument to be parsed
#' @param ind index of the argument to be parsed, starting from 1.
#' @param default default values to be returned if the argument is not provided
#' @param choices a character vector of accepted values; if a string outside the vector is provided, the function will stop and print error message
#' 
#' @details The parsing is performed at C-level. If the argument accepts only one value, users can also call argGet(opt, default=NULL, choices=NULL)
#'
#' @return  A character string representing the value of the argument
#'
#' @author Jitao David Zhang <jitao_david.zhang@roche.com>
#' @seealso \code{\link{argParse}}, \code{\link{argGet}}, and \code{\link{argPresent}}
#'
#' @examples
#' \dontrun{argGetPos("thresholds", ind=2)}
#'
#' 
argGetPos <- function(opt, ind=1L, default=NULL, choices=NULL) {
  if(isDebugging()) {
    message("[DEBUGGIING] The script is running in an interactive session, e.g. debugging mode. Default value is returned")
    return(default)
  }  
  if(argPresent(opt)) {
      res <- .Call("rarg_getPos", opt,as.integer(ind))
      if(!is.null(choices) && !res %in% choices)
          stop(sprintf("Option '%s' accepts only following values: %s",
                       opt, paste(choices, collapse=",")))
  } else {
      res <- default
  }
  return(res)
}

#' Parse an argument
#'
#' Get the value of an named argument
#'
#' @param opt name of the argument to be parsed
#' @param default default values to be returned if the argument is not provided
#' @param choices a character vector of accepted values; if a string outside the vector is provided, the function will stop and print error message
#' 
#' @details The parsing is performed at C-level. It is an abbreiviation of argGetPos(opt, ind=1, default=NULL, choices=NULL)
#'
#' @return  A character string representing the value of the argument
#'
#' @author Jitao David Zhang <jitao_david.zhang@roche.com>
#' @seealso \code{\link{argParse}}, \code{\link{argGetPos}}, and \code{\link{argPresent}}
#'
#' @examples
#' \dontrun{argGet("infile")}
#'
argGet <- function(opt, default=NULL, choices=NULL) {
  if(isDebugging()) {
    message("[DEBUGGIING] The script is running in an interactive session, e.g. debugging mode. Default value is returned")
    return(default)
  }
  if(argPresent(opt)) {
      res <- .Call("rarg_get", opt)
      if(!is.null(choices) && !res %in% choices) {
          stop(sprintf("Option '%s' accepts only following values: %s",
                       opt, paste(choices, collapse=",")))
      }
  } else {
      res <- default
  }
  return(res)
}
