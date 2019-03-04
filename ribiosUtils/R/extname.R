#### extname alternative implementation
#### gsub(".*\\.(\\w*)", "\\1", basename(as.character(x)))
#### Not good: fail when file does not have extname

#' @export basefilename
basefilename <- function(x, lower.case=FALSE) {
  if (!all(is.character(x))) 
    x <- as.character(x)
  sapply(strsplit(basename(x), "\\."), function(f) {
    res <- ifelse(length(f) == 1, f, paste(f[-length(f)],collapse="."))
    if(lower.case) res <- tolower(res)
    return(res)
  })
}





#' Get the base and extension(s) of file name(s)
#' 
#' Many files have base and extensions in their names, for instance for the
#' file \code{mybook.pdf}, the base is \code{mybook} and the extension is
#' \code{pdf}. \code{basefilename} \code{extname} functions extract these
#' information from one or more file names.
#' 
#' 
#' @aliases basefilename extname
#' @param x Character vector of file names; other classes will be coereced to
#' characters
#' @param ifnotfound If no extension name was found, the value to be returned.
#' Default is \code{NA}
#' @param lower.case Logical, should the names returned in lower case?
#' @return The base file name or the extension as characters, of the same
#' length as the input file name character. In case that a file name does not
#' contain a extension, \code{NA} will be returned.
#' @note In case there are multiple dots in the input file name, the last field
#' will be taken as the extension, and the rest as the base name. For instance
#' for file \code{test.out.txt}, returned base name is \code{test.out} and
#' extension is \code{txt}.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' extname("mybook.pdf")
#' extname("sequence.in.fasta")
#' extname(c("/path/mybook.pdf", "test.doc"))
#' extname("README")
#' extname("README", ifnotfound="")
#' extname("/path/my\ home/Holiday Plan.txt")
#' 
#' basefilename("mybook.pdf")
#' basefilename("sequence.in.fasta")
#' basefilename(c("/path/mybook.pdf", "test.doc"))
#' basefilename("README")
#' basefilename("/path/my\ home/Holiday Plan.txt")
#' 
#' basefilename("myBook.pdf", lower.case=TRUE)
#' extname("myBook.PDF", lower.case=TRUE)
#' 
#' @export extname
extname <- function(x, ifnotfound=NA, lower.case=FALSE) {
  ## NOT GOOD: without extension names will return the whole name
  ## gsub(".*\\.(\\w*)", "\\1", basename(as.character(x)))
  if(!all(is.character(x))) x <- as.character(x)
  sapply(strsplit(basename(x), "\\."), function(f) {
    res <- ifelse(length(f)==1, ifnotfound, f[length(f)])
    if(lower.case) res <- tolower(res)
    return(res)
  })
}
