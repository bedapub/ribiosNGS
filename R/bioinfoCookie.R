bioinfoCookie <- function(force.refresh=FALSE) {
  ## examine environment
  CACHE_VAR <- "RIBIOS_BIOINFO_COOKIE"
  biocookie <- unname(Sys.getenv("RIBIOS_BIOINFO_COOKIE"))
  if(!identical(biocookie, "") && !force.refresh) return(biocookie)

  ## no existing bioinfo cookie
  out <- .Call("get_bioinfo_cookie")
  rawCookie <- grep("^Set-Cookie",strsplit(out, "\r\n")[[1]],value=TRUE)
  if(length(rawCookie)<1)
    stop("Failed to get cookie, please contact the developer. Debugging information: ", out)
  hot.cookie <- gsub("Set-Cookie: ", "", rawCookie[[1]])
  Sys.setenv("RIBIOS_BIOINFO_COOKIE"=hot.cookie)
  return(hot.cookie)
}
