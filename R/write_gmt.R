write_gmt <- function(gmt, file, description=NULL) {
  isGeneList <- all(sapply(gmt,function(x) class(x)!="list"))
  isGmtList <- all(sapply(gmt, function(x) class(x)=="list" & length(x)==3))
  if(isGeneList) {
    gsets <- names(gmt)
    if(length(description)!=length(gmt))
      description <- rep(description, length(gmt)/length(description))
    gout <- lapply(seq(along=gmt),function(i)
                   list(name=gsets[i], description=description[i], genes=as.character(gmt[[i]])))
  } else if (isGmtList) {
    gout <- gmt
  } else {
    stop("Unrecognized GMT list: it is either a list of gene symbols, or a list of a three-element list with name, description, and genes")
  }
    
  invisible(.Call("c_write_gmt", gout, as.character(file)))
}
