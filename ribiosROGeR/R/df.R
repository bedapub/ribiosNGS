#' Write  data.frame to  a database connection
#'
#' @param conn A database connection
#' @param data.frame The \code{data.frame} to be written
#' @param tableName The name of the table
#' @param row.names Logical, whether \code{row.names} are to be written
#' @param overwrite Logical, whether the table should be overwritten in case it exists
#' @param append Logical, whether the table should be appended in case it exists
#' @param ... Other parameters passed to \code{dbWriteTable}
#'
#' Compared with \code{\link{dbWriteTable}}, \code{writeDfToDb} checks the
#' parameters and returns the last row ID.
#'
#' @return \code{writeDfToDb} returns the last insert Row ID (currently only
#' supporting SQLite).
#'
#' @examples
#' library(RSQLite)
#' myList <- list(list(MyTeam="HSV", Score=18),
#'                list(MyTeam="BVB", Score=16))
#' myTestDf <- data.frame(Number=c(3,4),
#'                        Complex=I(lapply(myList, function(x) { serialize(x, NULL)})))
#' myCon <- dbConnect(SQLite(), ":memory:")
#' writeDfToDb(myCon, myTestDf, tableName="testDataFrame")
#' myTestDfOut <- readDfFromDb(myCon, "testDataFrame")
#' myTestDfOut
#' unserialize(myTestDfOut$Complex[[1]])
#' testthat::expect_equal(myTestDf, myTestDfOut)
#'
#' @import RSQLite
#' @import DBI
#' @importFrom testthat expect_equal
#' @importFrom DBI dbWriteTable dbGetQuery dbReadTable
#' @importFrom methods is
#' @export
#'
writeDfToDb <- function(conn, data.frame,  tableName="testDf",
                        row.names=FALSE,
                        overwrite=FALSE, append=TRUE, ...) {
  stopifnot(is.data.frame(data.frame))
  stopifnot(!is.null(conn))
  stopifnot(!is.null(tableName) & !is.na(tableName))
  stopifnot(!(overwrite && append))

  dbWriteTable(conn=conn, name=tableName, value=data.frame,
               overwrite = overwrite,
               append=append,
               row.names = row.names,
               ...)
  if(is(conn, "SQLiteConnection")) {
    res <- dbGetQuery(conn, "SELECT last_insert_rowid();")[1,1]
  } else {
    warning(sprintf("last_insert_rowid not implemented for %s yet",
                    class(conn)))
    res <- NULL
  }
  return(invisible(res))
}

#' Read data.frame from a database connection
#'
#' @param conn A database connection
#' @param tableName The name of the table
#' @param row.names Logical, whether \code{row.names} are to be written
#' @param ... Other parameters passed to \code{dbReadTable}
#'
#' Compared with \code{\link{dbReadTable}}, \code{readDfFromDb} handles
#' \code{raw} vectors better so that printing the output \code{data.frame}
#' will not crash the R (or RStudio) session.
#'
#' @return \code{readDfFromDb} returns a data.frame object
#'
#' @examples
#' library(RSQLite)
#' myList <- list(list(MyTeam="HSV", Score=18),
#'                list(MyTeam="BVB", Score=16))
#' myTestDf <- data.frame(Number=c(3,4),
#'                        Complex=I(lapply(myList, function(x) { serialize(x, NULL)})))
#' myCon <- dbConnect(SQLite(), ":memory:")
#' writeDfToDb(myCon, myTestDf, tableName="testDataFrame")
#' myTestDfOut <- readDfFromDb(myCon, "testDataFrame")
#' myTestDfOut
#' unserialize(myTestDfOut$Complex[[1]])
#' testthat::expect_equal(myTestDf, myTestDfOut)
#'
#' @import RSQLite
#' @import DBI
#' @importFrom testthat expect_equal
#' @importFrom DBI dbWriteTable dbGetQuery dbReadTable
#' @importFrom methods is
#' @export
#'
readDfFromDb <- function(conn, tableName, row.names=FALSE, ...) {
  df <- dbReadTable(conn=conn, name=tableName,
                    row.names = row.names, ...)
  types <- sapply(df, function(x) typeof(x[[1]]))
  isRaw <- types=="raw"
  if(any(isRaw)) {
    for(i in which(isRaw)) {
      attr(df[[i]], "class") <- "AsIs"
    }
  }
  return(df)
}

#' Convert factor columns in a data.frame into character strings
#'
#' @param df A data.frame
#'
#' @examples
#' exampleDf <- data.frame(Teams=c("HSV", "FCB", "FCB", "HSV"),
#'              Player=c("Mueller", "Mueller", "Robben", "Holtby"),
#'              Scores=c(3.5, 1.5, 1.5, 1.0), stringsAsFactors=TRUE)
#' strDf <- dfFactor2Str(exampleDf)
#' stopifnot(identical(strDf[,1], c("HSV", "FCB", "FCB", "HSV")))
#' stopifnot(identical(exampleDf[,1], factor(c("HSV", "FCB", "FCB", "HSV"))))
#'
#' @export
dfFactor2Str <- function(df) {
  isFactor <- sapply(df, is.factor)
  if(any(isFactor)) {
    ind <- which(isFactor)
    for(i in ind)
      df[,i] <- as.character(df[,i])
  }
  return(df)
}

#' Convert logical values in a data.frame into integers
#'
#' @param df A data.frame
#'
#' @examples
#' exampleDf <- data.frame(Teams=c("HSV", "FCB", "FCB", "HSV"),
#'              Player=c("Mueller", "Mueller", "Robben", "Holtby"),
#'              IsPlaying=c(TRUE, FALSE, TRUE, TRUE),
#'              Scores=c(3.5, 1.5, 1.5, 1.0), stringsAsFactors=TRUE)
#' intDf <- dfLogical2Int(exampleDf)
#' stopifnot(identical(intDf[,3], c(1L, 0L, 1L, 1L)))
#'
#' @export
dfLogical2Int <- function(df) {
  isLogical <- sapply(df, is.logical)
  if(any(isLogical)) {
    ind <- which(isLogical)
    for(i in ind)
      df[,i] <- as.integer(df[,i])
  }
  return(df)
}
