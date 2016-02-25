sortByDimnames <- function(x,row.decreasing=FALSE, col.decreasing=FALSE) {
  x <- x[order(rownames(x), decreasing=row.decreasing),]
  x <- x[, order(colnames(x), decreasing=col.decreasing)]
  return(x)
}

asNumMatrix <- function(x) {
  mat <- apply(x, 2, as.numeric)
  dimnames(mat) <- dimnames(x)
  return(mat)
}
atofMatrix <- function(data.frame) {
  .Deprecated("asNumMatrix",
              package="ribiosUtils")
  asNumMatrix(data.frame)
}
stringDataFrame2numericMatrix <- function(data.frame) {
  .Deprecated("asNumMatrix",
              package="ribiosUtils")
  asNumMatrix(data.frame)
}

putColsFirst <- function(data.frame, columns) {
  stopifnot(all(columns %in% colnames(data.frame)))
  stopifnot(is.data.frame(data.frame) | is.matrix(data.frame))
  data.frame <- data.frame[,c(columns,
                              setdiff(colnames(data.frame), columns))]
  return(data.frame)
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
    isMatInput <- is.matrix(data.frame)
    if(isMatInput) {
        data.frame <- data.frame(data.frame, check.names=FALSE, check.rows=FALSE, stringsAsFactors=FALSE)
    }
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
    if(isMatInput) {
        res <- as.matrix(res)
    }
    return(res)
}

dfFactor <- function(df, sample.group) {
  if(!is.data.frame(df))
    stop("The function takes a data.frame as input. Don't pass the ExpressionSet object\n")
  if(length(sample.group)==1 & is.character(sample.group)) {
    stopifnot(sample.group %in% colnames(df))
    fac <- df[, sample.group]
  } else if (length(sample.group)==1 & is.numeric(sample.group)) {
    sample.group <- as.integer(sample.group)
    stopifnot(sample.group>=1L & sample.group <= ncol(df))
    fac <- df[, sample.group]
  } else {
    stopifnot(length(sample.group) == nrow(df))
    fac <- sample.group
  }
  if(!is.factor(fac)) fac <- factor(fac)
  return(fac)
}

## variable columns
isVarCol <- function(df) return(apply(df, 2L, ulen) > 1)
isInvarCol <- function(df) !isVarCol(df)
removeInvarCol <- function(df) df[,isVarCol(df), drop=FALSE]
