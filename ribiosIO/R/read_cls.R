#' Write a factor in the CLS format
#' 
#' @aliases write_cls
#'
#' @param fac A factor
#' @param con Connection to write to
#' @param offset he integer representing the first level, default is set to 0, for some software it can be set to 1
#' @param sep Separator used in the CLS format, can be '\\t' (recommended) or ' ' (not to be used when space exists in levels)
#' @note
#' The original CLS format specifies that both tab or space can be used as separators.
#' This makes it unable to represent factors with sapces in levels. In order to accomodate
#' CLS format for these factors, we propose using tab as separators in CLS files when encoding factors
#' in R. The default setting of \code{read_factor} and \code{write_factor} uses tab.
#'
#' @seealso \code{\link{read_factor}}
#'
#' @examples
#' set.seed(1887)
#' tempfac <- factor(sample(LETTERS, 30, replace=TRUE), levels=sample(LETTERS))
#' tempfile <- tempfile()
#' write_factor(tempfac, tempfile)
#' readLines(tempfile)
#' stopifnot(identical(tempfac, read_factor(tempfile)))
write_factor <- function(fac, con=stdout(),offset=0, sep=c("\t", " ")) {
    stopifnot(is.factor(fac))
    sep <- match.arg(sep)
    levels <- levels(fac)

    firstLine <- paste(length(fac), length(levels), "1", sep=sep)
    secondLine <- paste("#", paste(levels(fac), collapse=sep), sep=sep)
    thirdLine <- paste(as.integer(fac)-1+offset, collapse=sep)
    res <- c(firstLine, secondLine, thirdLine)
    writeLines(res, con=con)
}

write_cls <- write_factor

#' Read in a factor writtin in the CLS format
#'
#' @param con File or connection to read file from
#' @param offset  The integer representing the first level, default is set to 0, for some software it can be set to 1
#' @param sep Separator used in the CLS format, can be '\\t' (recommended) or '\\s' (not to be used when space exists in levels)
#'
#' @note
#' The original CLS format specifies that both tab or space can be used as separators.
#' This makes it unable to represent factors with sapces in levels. In order to accomodate
#' CLS format for these factors, we propose using tab as separators in CLS files when encoding factors
#' in R. The default setting of \code{read_factor} and \code{write_factor} uses tab.
#'
#' @seealso \code{\link{write_factor}}
#' 
#' @examples
#' set.seed(1887)
#' tempfac <- factor(sample(LETTERS, 30, replace=TRUE), levels=sample(LETTERS))
#' tempfile <- tempfile()
#' write_factor(tempfac, tempfile)
#' stopifnot(identical(tempfac, read_factor(tempfile)))
read_factor <- function(con=stdin(), offset=0, sep=c("\t", "\\s")) {
    sep <- match.arg(sep)
    lns <- readLines(con)
    stopifnot(length(lns) == 3)
    indims <- as.integer(strsplit(lns[1L], sep)[[1]])
    if (!(length(indims) == 3L & indims[3L] == 1L)) {
        stop("The first line of cls file is mal-formated.\n")
    }
    slevels <- strsplit(lns[2L], sep)[[1L]]
    if (!(identical(slevels[1], "#") & length(slevels) == indims[2L] + 1L)) {
        stop("The label line of cls file has fewer classes than indicated.\n")
    }
    slevels <- slevels[-1L]
    fn <- as.integer(strsplit(lns[3], sep)[[1]])-offset+1
    if (length(fn) != indims[1L]) {
        stop("In cls file there are number-indexed samples than indicated in the header line.\n ")
    }
    sf <- factor(slevels[fn], levels = slevels)
    return(sf)
}

#' Read cls files
#'
#' @param cls.file CLS file name
#' @param offset The integer representing the first level, default is set to 0, for some software it can be set to 1
#'
#' It reads CLS format files with space or tab as separators.
#'
#' @note
#' The original CLS format specifies both tab or space can be used as separators.
#' This makes it unable to represent factors with sapces in levels. In order to accomodate
#' CLS format for these factors, we propose using tab as separators in CLS files when encoding factors
#' in R.
#'
#' Because of this, only use \code{read_cls} if you are sure that no level string contain space(s).
#' Otherwise, use read_factor and write_factor instead: they can handle such strings
#'
#' @seealso
#' \code{\link{read_factor}} and \code{\link{write_factor}}
#'
#' @examples
#' idir <- system.file("extdata", package="ribiosIO")
#' sample.cls <- read_cls(file.path(idir, "test.cls"))
#' expFac <- factor(c("Case", "Control")[c(1,0,1,0,0,1,0,0,0,1,0,1,0,0,0,1,0,1,0,0,1,1,1,1,0,0)+1],
#' levels=c("Case", "Control"))
#' stopifnot(identical(sample.cls, expFac))
read_cls <- function (cls.file, offset=0) {
    read_factor(con=cls.file, offset=offset, sep="\\s")
}
