#' Sort matrix by dim names
#' 
#' Rearrange rows and columns of a matrix by dim names
#' 
#' 
#' @param x A matrix or data.frame
#' @param row.decreasing Logical, whether rows should be sorted decreasingly
#' @param col.decreasing Logical, whether columns should be sorted decreasingly
#' @return Resorted matrix or data frame
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' testMat <- matrix(1:16, nrow=4, dimnames=list(c("B", "D", "A", "C"), c("t", "f", "a", "g")))
#' sortByDimnames(testMat)
#' sortByDimnames(testMat, row.decreasing=TRUE, col.decreasing=FALSE)
#' 
#' @export sortByDimnames
sortByDimnames <- function(x,row.decreasing=FALSE, col.decreasing=FALSE) {
  x <- x[order(rownames(x), decreasing=row.decreasing),]
  x <- x[, order(colnames(x), decreasing=col.decreasing)]
  return(x)
}

#' Convert string-valued data frame or matrix into a numeric matrix
#' 
#' 
#' @aliases asNumMatrix atofMatrix stringDataFrame2numericMatrix
#' @param x A data.frame or matrix, most likely with string values
#' @return A numeric matrix with the same dimension
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' testDf <- data.frame(a=c("2.34", "4.55"), b=c("7.33", "9.10"))
#' asNumMatrix(testDf)
#' 
#' testMatrix <- matrix(c("2.34", "4.55", "9E-3","-2.44", "7.33", "9.10"), nrow=2)
#' asNumMatrix(testMatrix)
#' 
#' @export asNumMatrix
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





#' Rearrange columns to put some columns to far left
#' 
#' This function is helpful to export tables where certain columns are desired
#' to be placed to the most left of the data.frame
#' 
#' 
#' @param data.frame Data.frame
#' @param columns Character vector, names of columns which are to be put to the
#' left
#' @return data.frame with re-arranged columns
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' clubs <- data.frame(Points=c(21,23,28,24), Name=c("BVB", "FCB", "HSV",
#' "FCK"), games=c(12,11,11,12))
#' putColsFirst(clubs, c("Name"))
#' putColsFirst(clubs, c("Name", "games"))
#' 
#' @export putColsFirst
putColsFirst <- function(data.frame, columns) {
  stopifnot(all(columns %in% colnames(data.frame)))
  stopifnot(is.data.frame(data.frame) | is.matrix(data.frame))
  data.frame <- data.frame[,c(columns,
                              setdiff(colnames(data.frame), columns))]
  return(data.frame)
}





#' Remove columns
#' 
#' Remove columns from a data.frame object
#' 
#' The function is equivalent to the subsetting operation with brackets. It
#' provides a tidy programming interface to manupulate data.frames.
#' 
#' @param data.frame data.frame
#' @param columns names of columns to be removed
#' @param drop Logical, whether the matrix should be dropped to vector if only
#' one column is left
#' @return data.frame with specified columns removed
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' clubs <- data.frame(Points=c(21,23,28,24), Name=c("BVB", "FCB", "HSV",
#' "FCK"), games=c(12,11,11,12))
#' removeColumns(clubs,c("Name"))
#' 
#' @export removeColumns
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




#' Replace column names in data.frame
#' 
#' Replace column names in data.frame
#' 
#' 
#' @aliases replaceColumnName replaceColumnNames
#' @param data.frame A data.frame
#' @param old.names Old column names to be replaced
#' @param new.names New column names
#' @return Data.frame with column names updated
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' clubs <- data.frame(Points=c(21,23,28,24), Name=c("BVB", "FCB", "HSV",
#' "FCK"), games=c(12,11,11,12))
#' replaceColumnName(clubs, c("Points", "games"), c("Punkte", "Spiele"))
#' 
#' @export replaceColumnName
replaceColumnName <- function(data.frame, old.names, new.names) {
  col.names <- colnames(data.frame)
  new.col.names <- replaceByMatch(col.names, old.names, new.names)
  colnames(data.frame) <- new.col.names
  return(data.frame)
}
replaceColumnNames <- function(...) {
    .Deprecated("replaceColumnName",
                package="ribiosUtils")
    replaceColumnName(...)
}
      




#' Sort data.frame rows by values in specified columns
#' 
#' Sort rows of an \code{data.frame} by values in specified columns.
#' 
#' Columns can be specified by integer indices, logical vectors or character
#' names.
#' 
#' @param data.frame A \code{data.frame} object
#' @param columns Column name(s) which sould be ordered
#' @param na.last Logical, whether NA should be sorted as last
#' @param decreasing Logical, whether the sorting should be in the decreasing
#' order
#' @param orderAsAttr Logical, whether the order index vectors should be
#' returned in the attribute \dQuote{order} of the sorted \code{data.frame}
#' @return Sorted \code{data.frame}
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' sample.df <- data.frame(teams=c("HSV", "BVB", "FCB", "FCN"),pts=c(18,17,17,9), number=c(7,7,6,6))
#' sortByCol(sample.df, 1L)
#' sortByCol(sample.df, 1L, decreasing=FALSE)
#' 
#' sortByCol(sample.df, c(3L, 1L))
#' sortByCol(sample.df, c(3L, 1L), decreasing=FALSE)
#' sortByCol(sample.df, c(3L, 2L))
#' 
#' sortByCol(sample.df, c(TRUE, FALSE, TRUE))
#' 
#' sortByCol(sample.df, c("teams", "pts"))
#' sortByCol(sample.df, c("pts", "number", "teams"))
#' sortByCol(sample.df, c("pts", "teams", "number"))
#' 
#' @export sortByCol
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





#' Get a factor vector for a data.frame
#' 
#' The function try to assign a factor vector for a \code{data.frame} object.
#' See details below.
#' 
#' The function tries to get a factor vector of the same length as the number
#' of rows in the \code{data.frame}. The determination is done in the following
#' order: Step 1: It tries to find a column in the \code{data.frame} with the
#' name as given by \code{sample.group}. If found, this column is transformed
#' into a factor if not and returned.  Step 2: It tries to interpret the
#' \code{sample.group} as an integer, as the index of the column in the
#' \code{data.frame} giving the factor.  Step 3: When \code{sample.group}
#' itself is a vector of the same length as the data.frame, it is cast to
#' factor when it is still not and returned.
#' 
#' Otherwise the program stops with error.
#' 
#' @param df A \code{data.frame}
#' @param sample.group A character, number or a vector of factors, from which
#' the factor vector should be deciphered. See details below.
#' @return A factor vector with the same length as the \code{data.frame}
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' df <- data.frame(gender=c("M", "M", "F", "F", "M"),
#' age=c(12,12,14,12,14), score=c("A", "B-", "C", "B-", "A"))
#' dfFactor(df, "gender")
#' dfFactor(df, "score")
#' dfFactor(df, 1L)
#' dfFactor(df, 2L)
#' dfFactor(df, df$score)
#' \dontrun{dfFactor(df, "mark")}
#' 
#' @export dfFactor
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

## match column names





#' Match a given vector to column names of a data.frame or matrix
#' 
#' Match a given vector to column names of a data.frame or matrix
#' 
#' 
#' @param data.frame.cols column names of a data.frame. One can also provide a
#' data.frame, which may however cause worse performance since the data.frame
#' is copied
#' @param reqCols required columns
#' @param ignore.case logical, whether the case is considered
#' @examples
#' 
#' myTestDf <- data.frame(HBV=1:3, VFB=0:2, BVB=4:6, FCB=2:4)
#' myFavTeams <- c("HBV", "BVB")
#' matchColumnName(myTestDf, myFavTeams)
#' myFavTeamsCase <- c("hbv", "bVb")
#' matchColumnName(myTestDf, myFavTeamsCase, ignore.case=TRUE)
#' ## NA will be returned in this case if ignore.case is set to FALSE
#' matchColumnName(myTestDf, myFavTeamsCase, ignore.case=FALSE)
#' 
#' @export matchColumnName
matchColumnName <- function(data.frame.cols, reqCols, ignore.case=FALSE) {
    if(is.data.frame(data.frame.cols))
        data.frame.cols <- colnames(data.frame.cols)
    if(ignore.case) {
        lowInputCol <- tolower(data.frame.cols)
        lowCol <- tolower(reqCols)
        res <- match(lowCol, lowInputCol)
    } else {
        res <- match(reqCols, data.frame.cols)
    }
    return(res)
}





#' Assert whether the required column names exist
#' 
#' Assert whether the required column names exist
#' 
#' The function calls \code{\link{matchColumnName}} internally to match the
#' column names.
#' 
#' @param data.frame.cols column names of a data.frame. One can also provide a
#' data.frame, which may however cause worse performance since the data.frame
#' is copied
#' @param reqCols required columns
#' @param ignore.case logical, whether the case is considered
#' @return If all required column names are present, their indices are returned
#' *invisibly*. Otherwise an error message is printed.
#' @examples
#' 
#' myTestDf <- data.frame(HBV=1:3, VFB=0:2, BVB=4:6, FCB=2:4)
#' myFavTeams <- c("HBV", "BVB")
#' assertColumnName(myTestDf, myFavTeams)
#' myFavTeamsCase <- c("hbv", "bVb")
#' assertColumnName(myTestDf, myFavTeamsCase, ignore.case=TRUE)
#' \dontrun{assertColumnName(myTestDf, myFavTeamsCase, ignore.case=TRUE)}
#' 
#' @export assertColumnName
assertColumnName <- function(data.frame.cols, reqCols, ignore.case=FALSE) {
    matchRes <- matchColumnName(data.frame.cols, reqCols, ignore.case=ignore.case)
    if(any(is.na(matchRes))) {
        moreThanOne <- sum(is.na(matchRes))
        stop("Following column%s not found: %s",
             ifelse(moreThanOne, "s were", " was"),
             paste(reqCols[is.na(matchRes)], collapse=","))
    }
    return(invisible(matchRes))
}





#' Subset a data.frame by column name, allowing differences in cases
#' 
#' Subset a data.frame by column name, allowing differences in cases
#' 
#' The function calls \code{\link{assertColumnName}} internally to match the
#' column names.
#' 
#' @param data.frame A data.frame object
#' @param reqCols required columns
#' @param ignore.case logical, whether the case is considered
#' @return If all required column names are present, the data.frame object will
#' be subset to include only these columns and the result data.frame is
#' returned. Otherwise an error message is printed.
#' @examples
#' 
#' myTestDf <- data.frame(HBV=1:3, VFB=0:2, BVB=4:6, FCB=2:4)
#' myFavTeams <- c("HBV", "BVB")
#' subsetByColumnName(myTestDf, myFavTeams)
#' myFavTeamsCase <- c("hbv", "bVb")
#' subsetByColumnName(myTestDf, myFavTeamsCase, ignore.case=TRUE)
#' \dontrun{subsetByColumnName(myTestDf, myFavTeamsCase, ignore.case=TRUE)}
#' 
#' @export subsetByColumnName
subsetByColumnName <- function(data.frame, reqCols, ignore.case=FALSE) {
    ind <- assertColumnName(data.frame, reqCols, ignore.case=ignore.case)
    res <- data.frame[,ind]
    colnames(res) <- reqCols
    return(res)
}

## variable columns
isVarCol <- function(df) return(apply(df, 2L, ulen) > 1)
isInvarCol <- function(df) !isVarCol(df)




#' Remove invariable columns from a data frame or matrix
#' 
#' Columns with one unique value are invariable. The functions help to remove
#' such columns from a data frame (or matrix) in order to highlight the
#' variables.
#' 
#' \code{removeInvarCol} the data frame removing invariable column(s).
#' 
#' \code{isVarCol} and \code{isInvarCol} are helper functions, returning a
#' logical vector indicating the variable and invariable columns respectively.
#' 
#' @aliases removeInvarCol isInvarCol isVarCol
#' @param df A data frame or matrix
#' @return \code{isVarCol} and \code{isInvarCol} return a logical vector
#' indicating the variable and invariable columns respectively.
#' 
#' \code{removeInvarCol} removes invariable columns.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#'  testDf <- data.frame(a=1:4, b=7, c=LETTERS[1:4])
#' isVarCol(testDf)
#' isInvarCol(testDf)
#' removeInvarCol(testDf)
#' 
#' @export removeInvarCol
removeInvarCol <- function(df) df[,isVarCol(df), drop=FALSE]





#' Transform a list of character strings into a data.frame
#' 
#' Transform a list of character strings into a data.frame
#' 
#' 
#' @param list A list of character strings
#' @param names Values in the 'Name' column of the result, used if the input
#' list has no names
#' @param col.names Column names of the \code{data.frame}
#' @examples
#' 
#' myList <- list(HSV=c("Mueller", "Papadopoulos", "Wood"), FCB=c("Lewandowski", "Robben", "Hummels"),
#'                BVB=c("Reus", "Goetze", "Kagawa"))
#' list2df(myList, col.names=c("Club", "Player"))
#' 
#' @export list2df
list2df <- function(list, names=NULL, col.names=c("Name", "Item")) {
  if(is.null(names))
    names <- names(list)
  if(is.null(names))
    stop("Parameter 'names' cannot be NULL if the list has NULL names")
  res <- data.frame(Name=rep(names, sapply(list, length)),
             Item=unlist(list), row.names=NULL)
  colnames(res) <- col.names
  return(res)
}
