## sofar only accepts character
reload <- function(pkg) {
  pkg <- as.character(substitute(pkg))
  name <- sprintf("package:%s", pkg)

  if(name %in% search())
    detach(name, unload=TRUE, character.only=TRUE)
  library(pkg, character.only=TRUE)
}

