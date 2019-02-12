#' System user name
#' @return System user name
#' @examples 
#' whoami()
#' @export
whoami <- function() return(unname(Sys.info()["user"]))
