argParse <- function(optargs, reqargs, usage=paste(scriptName(), "-h"), strict=TRUE) {
  if(isDebugging()) {
    message("[DEBUGGIING] The script is running in an interactive session, e.g. debugging mode")
    return(invisible(NULL))
  }
  allComm <- commandArgs(FALSE)
  if("--args" %in% allComm) {
    indFile <- grep("^--file=", allComm)
    allComm[indFile] <- gsub("^--file=", "", allComm[indFile]) ## script name
    allComm <- allComm[-c(1:(indFile-1))]
    comm <- allComm[!grepl("^--", allComm)]
  } else {
    comm <- allComm[-c(1:2)]
  }
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
  if(strict) {
    if(argc != res)
      qqmsg(usage)
    return(invisible(NULL))
  } else {
    if(argc >= (res+1)) {
      return(invisible(comm[(res+1):argc]))
    } else {
      return(NULL)
    }
  }
}

argIsInit <- function() .Call("rarg_isInit")

argPresent <- function(opt) .Call("rarg_present", opt)

argGetPos <- function(opt, ind=1L, default=NULL) {
  if(argPresent(opt))
    return(.Call("rarg_getPos", opt,as.integer(ind)))
  return(default)
}
argGet <- function(opt, default=NULL) {
  if(argPresent(opt))
    return(.Call("rarg_get", opt))
  return(default)
}
