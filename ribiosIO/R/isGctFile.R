#' Test a file is a GCT file or not
#'
#' @param file Character string, a file name
#' @param strict.column.names Logical, whether the names of the first two columns must be 'NAME(tab)Description'
#'
#' @details
#'  A file is a valid GCT file if it meets following three rules:
#' \enumerate{
#'   \item The first line of the file is \code{#1.2}
#'   \item The second line contains number of rows and number of
#'     columns, separated by a tab.
#'   \item The rest of file contain a rectangular matrix, with the first
#'     two columns named \code{NAME} and \code{Description}
#'     respectively.
#' }
#'
#' @return A logical value: \code{TRUE} means \code{file} is of the GCT format.
#' @references \url{http://www.broadinstitute.org/cancer/software/genepattern/file-formats-guide#GCT}
#' @seealso \code{\link{read_gct_matrix}} to read in GCT files
#' @examples
#' myInFile <- system.file("extdata/test.gct", package="ribiosIO")
#' isGctFile(myInFile)
#' myInfileLS <- system.file("extdata/test_lessStrict.gct", package="ribiosIO")
#' isGctFile(myInfileLS)
isGctFile <- function(file, strict.column.names=FALSE) {
  headers <- readLines(file, n=3)
  isFL <- grepl("^#1.2", headers[[1]])
  isSL <- grepl("^[0-9]*\\s[0-9]*\\s*$", headers[[2]])
  res <- isFL && isSL
  if(strict.column.names) {
      isTL <- grepl("^NAME\\sDescription\\s.*", headers[[3]], ignore.case=TRUE)
      res <- res && isTL
  }
  return(res)
}
