#' Write a list of strings in a tab-delimited file
#'
#' @param list A list of character strings
#' @param file A filename
#' @param names Names of the list; by default the names of the list
#' @param type Should list items written in columns or rows?
#' @param index Logical, should integer index be printed along the elements?
#'
#' @example
#' myList <- list("A"=LETTERS[3], "B"=LETTERS[4])
#' writeStrList(myList, file=tempfile())
#' writeStrList(myList, file=tempfile(), names=c("ListA", "ListB"))
#' writeStrList(myList, file=tempfile(), names=c("ListA", "ListB"), type="row")
#' writeStrList(myList, file=tempfile(), names=c("ListA", "ListB"), type="row", incdex=TRUE)
writeStrList <- function(list, file, names=names(list), type=c("column", "row"), index=FALSE) {
  type <- match.arg(type)
  stopifnot(!is.null(names))
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
      colnames(tbl) <- 1:nrow(tbl)
    }
    write.table(tbl, file=file,
                quote=FALSE, sep="\t", row.names=TRUE,
                col.names=index, dec=".")
  } else {
    stop("Should not be here")
  }
}
