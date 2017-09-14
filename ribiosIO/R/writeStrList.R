/#' Write a list of strings in a tab-delimited file
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
  maxlen <- max(sapply(list, length))
  flist <- lapply(list, function(x) c(x, rep("", maxlen-length(x))))
  if(type=="column") {
    tbl <- do.call(cbind,flist)
    colnames(tbl) <- names
    if(index) {
      rownames(tbl) <- 1:nrow(tbl)
    }
    writeMatrix(tbl, file, row.names=index)
  } else if (type=="row") {
    tbl <- do.call(rbind, flist)
    rownames(tbl) <- names
    if(index) {
      rownames(tbl) <- 1:nrow(tbl)
    }
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
