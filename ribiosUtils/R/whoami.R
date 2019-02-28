#' System user name
#' 
#' 
#' @return System user name
#' @examples
#' 
#' whoami()
#' 
#' @export whoami
whoami <- function() return(unname(Sys.info()["user"]))
