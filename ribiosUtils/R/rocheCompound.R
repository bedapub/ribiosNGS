#' Tell whether a character string is a Roche compound ID
#' @param str Character string(s)
#' @return A logical vector of the same length as \code{str}, indicating whether each element is a Roche compound ID or not.
#' 
#' Short versions (RO[1-9]{2,7}) are supported.
#' 
#' @examples
#' isRocheCompoundID(c("RO1234567", "RO-1234567", 
#'                    "RO1234567-000", "RO1234567-000-000",
#'                    "ROnoise-000-000"))
#' @export isRocheCompoundID
isRocheCompoundID <- function(str) {
  grepl("^RO-?{0,1}[0-9]{2,7}(-[0-9]{3})?(-[0-9]{3})?$", 
        as.character(str))
}

#' Extract core identifiers from Roche compound IDs
#' @param str Character strings
#' @param short Logical, if \code{TRUE}, the short version of Roche identifiers (\code{RO[0-9]{4}}) is returned. Default: \code{FALSE}
#' @return Core identifiers if the element is a Roche compound ID, the original element otherwise
#' Non-character input will be converted to character strings first.
#' @seealso \code{\link{isRocheCompoundID}}
#' @examples
#' rocheCore(c("RO1234567-001", "RO1234567-001-000", "RO1234567", 
#'     "ROnoise-001", "anyOther-not-affected"))
#' rocheCore(c("RO1234567-001", "RO1234567-001-000", "RO1234567",
#'     "ROnoise-001","anyOther-not-affected"), short=TRUE)
#' 
#' @export rocheCore
rocheCore <- function(str, short=FALSE) {
  str <- as.character(str)
  isRO <- isRocheCompoundID(str)
  res <- gsub("-[0-9]{3}$", "", as.character(str))
  res <- gsub("-[0-9]{3}$", "", res)
  if(short) {
    res <- gsub("RO[0-9]{3}([0-9]{4})$", "RO\\1", res)
  }
  res[!isRO] <- str[!isRO]
  return(res)
}
