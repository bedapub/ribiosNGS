#' Checks existing directory
#' 
#' Checks whether given character strings point to valid directories
#' 
#' \code{isDir} tests whether the given string represent a valid, existing
#' directory. \code{assertDir} performs a logical test, and stops the program
#' if the given string does not point to a given directory.
#' 
#' \code{checkDir} is synonymous to \code{isDir}
#' 
#' @aliases isDir checkDir assertDir
#' @param \dots One or more character strings giving directory names to be
#' tested
#' @return \code{isDir} returns logical vector.
#' 
#' \code{assertDir} returns an invisible \code{TRUE} if directories exist,
#' otherwise halts and prints error messages.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{file.info}}, \code{\link{checkFile}} and
#' \code{\link{assertFile}}
#' @examples
#' 
#' dir1 <- system.file(package="ribiosIO")
#' dir2 <- system.file(package="ribiosUtils")
#' 
#' isDir(dir1, dir2)
#' \dontrun{assertDir(dir1, dir2)}
#' 
#' @export isDir checkDir assertDir
isDir <- function(...) {
  x <- unlist(list(...))
  na.false(file.info(x)$isdir)
}

checkDir <- function(...) {
  x <- unlist(list(...))
  all(isDir(...))
}
assertDir <- function(...) {
  haltifnot(checkDir(...),
            msg="Not all directories exist\n")
  return(invisible(TRUE))
}


#' Create a directory if it does not exist, and then make sure the creation was
#' successful.
#' 
#' The function is particularly useful for scripting.
#' 
#' 
#' @param dir Directory name
#' @param showWarnings Passed to \link{dir.create}
#' @param recursive Passed to \link{dir.create}
#' @param mode Passed to \link{dir.create}
#' @return Directory name (invisible)
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' tempdir <- tempdir()
#' createDir(tempdir)
#' 
#' @export createDir
createDir <- function(dir,showWarnings=FALSE, recursive=FALSE, mode="0777") {
  if(!checkDir(dir))
    dir.create(path=dir, showWarnings=showWarnings, recursive=recursive, mode=mode)
  if(!checkDir(dir))
    stop("Directory access not possible: ", dir)
  return(invisible(dir))
}


#' Check whether file(s) exist
#' 
#' \code{checkFile} checks whether file exists, \code{assertFile} stops the
#' program if files do not exist
#' 
#' \code{assertFile} is often used in scripts where missing a file would cause
#' the script fail.
#' 
#' @aliases checkFile assertFile
#' @param \dots Files to be checked
#' @return \code{checkFile} returns logical vector. \code{assertFile} returns
#' an invisible \code{TRUE} if files exist, otherwise halts and prints error
#' messages.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{isDir}} and \code{\link{assertDir}}
#' @examples
#' 
#' myDesc <- system.file("DESCRIPTION", package="ribiosUtils")
#' myNEWS <- system.file("NEWS", package="ribiosUtils")
#' checkFile(myDesc, myNEWS)
#' assertFile(myDesc, myNEWS)
#' 
#' @export checkFile assertFile
checkFile <- function(...) {
  x <- unlist(list(...))
  all(file.exists(x))
}
assertFile <- function(...) {
  af <- checkFile(...)
  if(af) return(invisible(TRUE))
  
  x <- unlist(list(...))
  notfound <- x[!file.exists(x)]
  haltifnot(af,
            msg=paste(paste("File not found:", notfound, sep=""),
              collapse="\n"))
}
