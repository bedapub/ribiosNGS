matrix2longdf <- function(mat,
                          row.names, col.names,
                          longdf.colnames=c("row","column","value")) {
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
  res <- data.frame(row=rn,
                    column=cn,
                    value=value)
  colnames(res) <- longdf.colnames
  return(res)
}

##longdf2matrix <- function(df, row.col=1L, column.col=2L, value.col=3L)  {
##  sub <- df[, c(row.col, column.col, value.col)]
##  tbl <- reshape(sub, idvar=colnames(sub)[1L],
##                 timevar=colnames(sub)[2],
##                 v.names=colnames(sub)[3],
##                 direction="wide")
##  mat <- data.matrix(tbl[,-1L, drop=FALSE])
##  rownames(mat) <- tbl[,1L,drop=TRUE]
##  new.cols <- sapply(colnames(mat),
##                     function(x) paste(strsplit(x, "\\.")[[1]][-1], collapse="."))
##  colnames(mat) <- new.cols
##  return(mat)
##}

longdf2matrix <- function(df, row.col = 1L, column.col = 2L, value.col = 3L) {
    sub <- as.data.frame(df[, c(row.col, column.col, value.col)])
    subrows <- unique(sub[,1L])
    subcols <- unique(sub[,2L])
    r.ind <- match(sub[,1L], subrows)
    c.ind <- match(sub[,2L], subcols)
    m.ind <- (c.ind-1)*length(subrows) + r.ind
    if(length(m.ind)!=(length(subrows)+0.0)*length(subcols))
        warning("Missing values detected\n")
    mat <- matrix(NA, nrow=length(subrows), ncol=length(subcols))
    mat[m.ind] <- sub[,3L]
    rownames(mat) <- subrows
    colnames(mat) <- subcols
    return(mat)
}
