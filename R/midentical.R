midentical <- function(..., num.eq=TRUE, single.NA=TRUE, attrib.as.set=TRUE,
                       ignore.bytecode=TRUE, ignore.environment=FALSE) {
  li <- list(...)
  if(length(li)==1L) li <- li[[1L]]
  
  stopifnot(length(li)>=2L)
  res <- do.call(identical, list(li[[1L]], li[[2L]],
                                 num.eq=num.eq, single.NA=single.NA,
                                 attrib.as.set=attrib.as.set,
                                 ignore.bytecode=ignore.bytecode,
                                 ignore.environment=ignore.environment))
  if(length(li)>2L)
    for(i in 2L:(length(li)-1L)) {
      res <- res && do.call(identical,
                            list(li[[i]],li[[i+1]],
                                 num.eq=num.eq, single.NA=single.NA,
                                 attrib.as.set=attrib.as.set,
                                 ignore.bytecode=ignore.bytecode,
                                 ignore.environment=ignore.environment ))
      if(!res) return(FALSE)
    }
  return(res)
}
