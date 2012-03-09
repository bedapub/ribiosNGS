matrix2longdf <- function(mat,
                          row.names, col.names) {
  if(missing(row.names)) row.names <- rownames(mat)
  if(missing(col.names)) col.names <- colnames(mat)
  
  if(is.null(row.names)) row.names <- 1:nrow(mat)
  if(is.null(col.names)) col.names <- 1:ncol(mat)
  
  value <- as.vector(mat)
  if(length(row.names)!=nrow(mat))
    warning("row.names is inconsistent with the matrix dim")
  if(length(col.names)!=ncol(mat))
    warning("col.names is inconsistent with the matrix dim")
  
  rn <- rep(row.names, ncol(mat))
  cn <- rep(col.names, each=nrow(mat))
  return(data.frame(row=rn,
                    column=cn,
                    value=value))
}
