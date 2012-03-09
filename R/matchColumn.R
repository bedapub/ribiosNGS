matchColumnIndex <- function(vector,
                             data.frame, column,
                             multi=FALSE) {
  stopifnot(is.data.frame(data.frame))
  
  isColName <- column %in% colnames(data.frame)
  isColNum <- is.numeric(column)
  isColInd <- isColNum && (as.integer(column) %in% 1:ncol(data.frame))
  isColRow <- isColNum && identical(as.integer(column), 0L)
  stopifnot(isColName || isColInd || isColRow)
  
  if(isColRow) {
    target <- rownames(data.frame)
  } else {
    target <- data.frame[, column]
  }
  
  if(multi)
    index <- lapply(vector, function(x) {
      res <- target %in% x
      if(any(res))
        return(which(target %in% x))
      else
        return(NA)
    })
  else
    index <- match(vector, target)
  return(index)
}

matchColumn <- function(vector,
                        data.frame, column,
                        multi=FALSE) {
  index <- matchColumnIndex(vector, data.frame, column, multi=multi)
  if(multi) {
    vector <- rep(vector,
                  sapply(index,length))
    index <- unlist(index)
  }
  res <- data.frame[index,]
  if(!(is.numeric(column) && identical(as.integer(column), 0L))) {
    res[, column] <- vector
  } else {
    vec <- make.unique(vector)
    if(!any(is.na(vec)))
      rownames(res) <- vec
    else
      rownames(res) <- NULL
  }
  return(res)
}
