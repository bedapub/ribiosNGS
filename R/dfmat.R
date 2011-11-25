headhead <- function(x, m=6L, n=6L,...) {
  stopifnot(length(n) == 1L && length(m) == 1L)
  n <- if (n < 0L)
    max(ncol(x) + n, 0L)
  else
    min(n, ncol(x))

  m <- if (m < 0L)
    max(nrow(x) + m, 0L)
  else
    min(m, nrow(x))

  x[seq_len(m), seq_len(n), drop = FALSE]
}

tailtail <- function(x, m = 6L, n = 6L,
                     addrownums = TRUE,
                     addcolnums = TRUE,...) 
{
    stopifnot(length(m) == 1L & length(n) == 1L)
    mrx <- nrow(x)
    ncx <- ncol(x)
    m <- if (m < 0L) 
        max(mrx + m, 0L)
    else min(m, mrx)
    n <- if (n < 0L)
      max(ncx + n, 0L)
    else min(n, ncx)
    
    sel.row <- seq.int(to = mrx, length.out = m)
    sel.col <- seq.int(to = ncx, length.out = n)
    
    ans <- x[sel.row, sel.col, drop = FALSE]
    if (addrownums && is.null(rownames(x))) 
        rownames(ans) <- paste("[", sel.row, ",]", sep = "")
    if (addcolnums && is.null(colnames(x))) 
        colnames(ans) <- paste("[", sel.col, ",]", sep = "")
    ans
}

orderByDimnames <- function(x,row.decreasing=FALSE, col.decreasing=FALSE) {
  x <- x[order(rownames(x), decreasing=row.decreasing),]
  x <- x[, order(colnames(x), decreasing=col.decreasing)]
  return(x)
}

atofMatrix <- function(x) {
  mat <- apply(x, 2, as.numeric)
  dimnames(mat) <- dimnames(x)
  return(mat)
}

stringDataFrame2numericMatrix <- function(data.frame) {
  .Deprecated("atofMatrix",
              package="ribiosUtils")
  atofMatrix(data.frame)
}

putColsFirst <- function(data.frame, columns) {
  stopifnot(all(columns %in% colnames(data.frame)))
  stopifnot(is.data.frame(data.frame) | is.matrix(data.frame))
  data.frame <- data.frame[,c(columns,
                              setdiff(colnames(data.frame), columns))]
  return(data.frame)
}

placeColumnsFirst <- function(data.frame, columns) {
  .Deprecated("putColsFirst","ribiosUtils")
  putColsFirst(data.frame, columns)
}

removeColumns <- function(data.frame, columns, drop=FALSE) {
  col.names <- colnames(data.frame)
  if(!any(columns %in% col.names)) {
    warning("data.frame does not contain following columns:",
         setdiff(columns, col.names))
  }
  data.frame <- data.frame[,setdiff(col.names, columns), drop=drop]
  return(data.frame)
}

matchColumnIndex <- function(vector, data.frame, column) {
  stopifnot(is.data.frame(data.frame) & column %in% colnames(data.frame))
  index <- match(vector, data.frame[, column])
  return(index)
}
matchColumn <- function(vector, data.frame, column) {
  index <- matchColumnIndex(vector, data.frame, column)
  return(data.frame[index,])
}

## change column names
replaceByMatch <- function(vector, old.items, new.items) {
  stopifnot(all(old.items %in% vector))
  stopifnot(length(old.items)==length(new.items))
  vector.backup <- vector
  for(i in seq(along=old.items)) {
    vector[vector.backup == old.items[i]] <- new.items[i]
  }
  return(vector)
}
replaceColumnNames <- function(data.frame, old.names, new.names) {
  col.names <- colnames(data.frame)
  new.col.names <- replaceByMatch(col.names, old.names, new.names)
  colnames(data.frame) <- new.col.names
  return(data.frame)
}


sortByCol <- function (data.frame, columns,
                       na.last = TRUE,
                       decreasing = TRUE,
                       orderAsAttr=FALSE)  {
  if(all(is.character(columns))) {
    stopifnot(all(columns %in% colnames(data.frame)))
  } else if (!all(is.numeric(columns)) && !all(is.logical(columns))) {
    stop("'columns' must be one of the following data types: chracters, numeric and logical\n")
  }
    
  subdf <- data.frame[,columns,drop=FALSE]
  local.order <- function(...) order(..., na.last=na.last,decreasing=decreasing)
  ord <- do.call(local.order, subdf) ## see example(order)
  res <- data.frame[ord,,drop=FALSE]
  if(orderAsAttr)
    attr(res, "order") <- ord
  return(res)
}
