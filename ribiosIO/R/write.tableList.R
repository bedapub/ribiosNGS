write.tableList <- function(list, file.names, ...) {
  if(missing(file.names))
    file.names <- names(list)
  if(is.null(file.names) || length(list)!=length(file.names))
    stop("file.names must be of the same length as list")
  for(i in seq(along=list))
    write.table(list[[i]], file.names[[i]], ...)
}
