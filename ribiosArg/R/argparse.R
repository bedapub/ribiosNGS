#' Parser of command-line parameters in BIOS style
#' @aliases argIsInit
#' @aliases argPresent
#'
#' @param optargs String describing optional arguments. Syntax: \code{<optname1>[,paramcnt1] <optname2>[,paramcnt2]\dots}. Example: \dQuote{verbose outfile,1} means the command line has the syntax \code{prog [-verbose] [outfile name]}. It can be an empty string to express \dQuote{no options}. The value for \code{paramcnt} is 0.
#' @param reqargs String describining required arguments. Syntax: \code{<argname1> <argname2>\dots}. Example: \dQuote{infile outfile} means the command line has the syntax \code{prog [-infile ]infile [-outfile ]coutfile}. Even if it is empty, it is checked that at least one non-optional value is given.
#' @param usage A character string to be printed if the command-line option parsing fails
#' @param strict Logical, are extra un-prefixed parameters allowed? If set to \code{TRUE}, the un-prefixed parameters (which must be at the end of the command line) will be returned as a character vector.
#'
#' @details
#'  \code{argParse} must be called before \code{argGet},\code{argGetPos}
#'   , \code{argPresent}, or \code{argGetDefault}. It checks whether the command line syntax agrees
#'   with the specification of \code{optargs} and \code{reqargs}. If not,
#'   the \code{usage} message is printed and the program exists.
#' 
#'   \code{argPresent} returns a boolean value indicating whether the option is present or not.
#'   
#'   If the syntax was found correct, \code{argGetPos} can be called to
#'   fetch the \code{ind}th value of the option \code{opt} (indexing from 1). For instance,
#'   if the following option \code{-ranges 3 5} is defined,
#'   argGetPos(\dQuote{range}, 2) returns \code{5}. \code{argGet} is
#'   a shortcut to fetch the first element. If the opt is missing, the
#'   \code{default} value will be returned.
#'
#' @return
#' \code{argParse} is used for the side effects. If \code{strict} is set to
#' \code{TRUE}, an invisible \code{NULL} is returned; otherwise, extra
#' un-prefixed parameters are returned as an invisible character vector 
#'
#' \code{argGet} and \code{argGetPos} returns a character
#' string. \code{argPresent} returns a boolean value.
#'
#' In case of any error (wrong syntax, or not-existing option) the R
#' session quits while printing the error message.
#'
#' @importFrom ribiosUtils qqmsg
#' @export
#' @useDynLib ribiosArg, .registration=TRUE, .fixes="C_"
#' 
#' @examples
#' \dontrun{
#' argParse("verbose threshold,2", "infile outfile",
#'          usage="prog [-infile ]infile [-outfile ]outfile [-verbose] [-threshold MIN MAX]")
#' argIsInit()
#' argPresent("verbose")
#' }
#' 
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
  res <- .Call(C_rarg_parse,
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

argIsInit <- function() .Call(C_rarg_isInit)

#' Test whether the given option is present in the command line or not
#'
#' @param opt Character string, option name
#' 
#' @export
argPresent <- function(opt) {
    if(isDebugging()) {
        message("[DEBUGGIING] The script is running in an interactive session, e.g. debugging mode. FALSE is returned")
        return(FALSE)
    }
  .Call(C_rarg_present, opt)
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
      res <- .Call(C_rarg_getPos, opt,as.integer(ind))
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
      res <- .Call(C_rarg_get, opt)
      if(!is.null(choices) && !res %in% choices) {
          stop(sprintf("Option '%s' accepts only following values: %s",
                       opt, paste(choices, collapse=",")))
      }
  } else {
      res <- default
  }
  return(res)
}
