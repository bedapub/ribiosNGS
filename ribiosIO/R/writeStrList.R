#' Format a string list into a data.frame
#'
#' @param strList A list of character strings
#' @param colnames Column names of the resulting data.frame, by default the names of the list
#' @param index Logical value, whether the row.names attribute of the data.frame should be integer indexes
#'
#' @examples
#' myList <- list("A"=LETTERS[3:5], "B"=LETTERS[4])
#' strList2DataFrame(myList)
#' strList2DataFrame(myList, colnames=c("FirstColumn", "SecondColumn"))
#' strList2DataFrame(myList, colnames=c("FirstColumn", "SecondColumn"), index=TRUE)

strList2DataFrame <- function(strList, colnames=names(strList), index=FALSE) {
  maxlen <- max(sapply(strList, length))
  flist <- lapply(strList, function(x) c(x, rep("", maxlen-length(x))))
  tbl <- do.call(cbind,flist)
  colnames(tbl) <- colnames
  if(index) {
    rownames(tbl) <- 1:nrow(tbl)
  } else {
    rownames(tbl) <- NULL
  }
  return(tbl)
}

#' Write a list of strings in a tab-delimited file
#'
#' @param list A list of character strings
#' @param file A filename
#' @param names Names of the list; by default the names of the list
#' @param type Should list items written in columns or rows?
#' @param index Logical, should integer index be printed along the elements?
#'
#' @examples
#' myList <- list("A"=LETTERS[3:5], "B"=LETTERS[4])
#' writeStrList(myList, file=stdout())
#' writeStrList(myList, file=stdout(), names=c("ListA", "ListB"))
#' writeStrList(myList, file=stdout(), names=c("ListA", "ListB"), type="row")
#' writeStrList(myList, file=stdout(), names=c("ListA", "ListB"), type="row", index=TRUE)
#' writeStrList(myList, file=stdout(), names=c("ListA", "ListB"), type="column", index=TRUE)
writeStrList <- function(list, file, names=NULL, type=c("column", "row"), index=FALSE) {
  type <- match.arg(type)
  if(is.null(names))
      names <- names(list)
  if(is.null(names))
      stop("Input list must have valid names when the parameter 'names' is null")
  tbl <- strList2DataFrame(list, colnames=names, index=index)
  if(type=="column") {
    writeMatrix(tbl, file, row.names=index)
  } else if (type=="row") {
    tbl <- t(tbl)
    if(!index) {
        write.table(tbl, file=file,
                    quote=FALSE, sep="\t", row.names=TRUE,
                    col.names=FALSE, dec=".")
    } else {
        write.table(tbl, file=file,
                    quote=FALSE, sep="\t", row.names=TRUE,
                    col.names=NA, dec=".")
    }
  } else {
    stop("Should not be here")
  }
}
