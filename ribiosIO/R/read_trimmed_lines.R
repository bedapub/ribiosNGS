#' Read lines, thereby trimming empty spaces around the strings and removing empty lines
#' @param file A text file
#' @param skipNul Skip NULL line (passed to \code{readLines})
#' @param ... Other paratmers than \code{skipNul} passed to readLines
#' 
#' @examples
#' lines <- "  ABC \n\tHBV\n\nFCB  \n\n"
#' trimmedLines <- read_trimmed_lines(textConnection(lines))
#' stopifnot(identical(trimmedLines, c("ABC", "HBV", "FCB")))
read_trimmed_lines <- function(file, skipNul=TRUE, ...) {
    lines <- readLines(file, skipNul=skipNul, ...)
    lines <- trim(lines)
    lines <- setdiff(lines, "")
    return(lines)
}
