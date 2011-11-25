## sofar only accepts character
reload <- function(pkg, pos=2) {
  name <- sprintf("package:%s", pkg)

  if(name %in% search())
    detach(name, unload=TRUE, character.only=TRUE)
  library(pkg, character.only=TRUE)
}

