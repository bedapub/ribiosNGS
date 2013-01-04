argParse <- function(optargs, reqargs, usage=paste(scriptName(), "-h")) {
    allComm <- commandArgs(FALSE)
  if("--args" %in% allComm) {
    indFile <- grep("^--file=", allComm)
    allComm[indFile] <- gsub("^--file=", "", allComm[indFile]) ## script name
    allComm <- allComm[-c(1:(indFile-1))]
    comm <- allComm[!grepl("^--", allComm)]
  } else {
    comm <- allComm[-c(1:2)]
  }
  argc <- as.integer(length(comm))
  res <- .Call("rarg_parse",
               argc,
               as.character(comm),
               as.character(optargs),
               as.character(reqargs),
               as.character(usage))
  if(argc != res) {
    cat(usage,"\n")
    quit(save="no")
  }
  return(invisible(res))
}

argGet <- function(opt) .Call("rarg_get", opt)
argGetPos <- function(opt, ind=1L) .Call("rarg_getPos", opt,as.integer(ind))
argPresent <- function(opt) .Call("rarg_present", opt)
