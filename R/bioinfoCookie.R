bioinfoCookie <- function(force.refresh=FALSE) {
  ## examine environment
  CACHE_VAR <- "RIBIOS_BIOINFO_COOKIE"
  biocookie <- unname(Sys.getenv("RIBIOS_BIOINFO_COOKIE"))
  if(!identical(biocookie, "") && !force.refresh) return(biocookie)

  ## no existing bioinfo cookie
  out <- .Call("get_bioinfo_cookie")
  raw.cookie <- grep("^Set-Cookie",strsplit(out, "\r\n")[[1]],value=TRUE)[[1]]
  hot.cookie <- gsub("Set-Cookie: ", "", raw.cookie)
  Sys.setenv("RIBIOS_BIOINFO_COOKIE"=hot.cookie)
  return(hot.cookie)
}
