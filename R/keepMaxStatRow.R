isMaxStatRow <- function (matrix,
                          keys,
                          keepNArows = TRUE,
                          stat = function(x) mean(x, na.rm = TRUE),
                          ...) 
{
  if (!is.matrix(matrix)) 
    matrix <- as.matrix(matrix)
  if (!is.character(keys)) 
    keys <- as.character(keys)
  haltifnot(!missing(keys) && identical(length(keys), nrow(matrix)), 
            msg = "'keys' must be of the same length as the row number of the matrix")
  if (is.character(stat)) 
    stat <- get(stat, envir = parent.frame())
  haltifnot(is.function(stat), msg = "'stat' must be either a function for some certain statistic, e.g. sd, or the name of such a function\n")
  rows.has.index <- !is.na(keys) & keys != ""
  matrix.indexed <- matrix[rows.has.index, , drop = FALSE]
  matrix.indexed.rowStat <- apply(matrix.indexed, 1L, stat,...)
  if (any(is.na(matrix.indexed.rowStat))) 
    warning("Statistics of some rows are NA and they will be discarded. You may want to double check this.\n")
  matrix.indexed.fac <- factor(keys[rows.has.index])
  rows.by.index <- split(1:nrow(matrix.indexed), matrix.indexed.fac)
  stat.by.index <- split(matrix.indexed.rowStat, matrix.indexed.fac)
  max.rows <- sapply(1:nlevels(matrix.indexed.fac), function(x) rows.by.index[[x]][which.max(stat.by.index[[x]])])
  if (is.list(max.rows)) 
    max.rows <- unlist(max.rows)
  matrix.remain <- rep(FALSE, nrow(matrix))
  if (keepNArows) 
    matrix.remain[!rows.has.index] <- TRUE
  matrix.remain[rows.has.index][max.rows] <- TRUE
  return(matrix.remain)
}

keepMaxStatRowInd <- function(matrix,
                              keys,
                              keepNArows = TRUE,
                              stat = function(x) mean(x, na.rm = TRUE),
                              ...) {
  which(isMaxStatRow(matrix=matrix,
                     keys=keys,
                     keepNArows=keepNArows,
                     stat=stat, ...))
}

keepMaxStatRow <- function(matrix, keys, keepNArows=TRUE,
                           stat=function(x) mean(x, na.rm=TRUE),
                           levels=c("rownames", "attribute", "discard"),
                           ...) {
  levels <- match.arg(levels)
  matrix.remain <- isMaxStatRow(matrix=matrix, keys=keys,
                                keepNArows=keepNArows,
                                stat=stat, ...)
  res <- matrix[matrix.remain,,drop=FALSE]
  if(levels!="discard") {
    newnames <- keys[matrix.remain]
    if(levels=="rownames") {
      rownames(res) <- newnames
    } else if (levels=="attribute") {
      attr(res, "levels") <- newnames
    }
  }
  return(res)
}
