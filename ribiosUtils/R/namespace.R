#' Reload a package
#' 
#' Reload a package by first detaching and loading the library.
#' 
#' 
#' @param pkg Character string, name of the package
#' @return Side effect is used.
#' @note So far only character is accepted
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{detach}} and \code{\link{library}}
#' @examples
#' 
#' \dontrun{
#' library(ribiosUtils)
#' reload(ribiosUtils)
#' }
#' 
#' @export reload
reload <- function(pkg) {
  pkg <- as.character(substitute(pkg))
  name <- sprintf("package:%s", pkg)

  if(name %in% search())
    detach(name, unload=TRUE, character.only=TRUE)
  library(pkg, character.only=TRUE)
}

