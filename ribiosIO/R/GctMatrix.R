#' Create a GctMatrix object
#' @param matrix A numeric matrix
#' @param desc Character vector of feature description, length must equal nrow of the matrix
#' @return A \code{GctMatrix} object
#' @examples
#' m1 <- matrix(1:6, nrow=3, dimnames=list(sprintf("G%d", 1:3), sprintf("S%d", 1:2)))
#' m2 <- matrix(c(9:7, 12:10), nrow=3, dimnames=list(sprintf("G%d", 3:1), sprintf("S%d", 3:4)))
#' gm1 <- GctMatrix(m1, desc=sprintf("Gene%d", 1:3))
#' gm2 <- GctMatrix(m2, desc=sprintf("Gene%d", 3:1))
#' print(gm1)
#' print(gm2)
GctMatrix <- function(matrix, desc) {
  if(!missing(desc))
    stopifnot(length(desc)==nrow(matrix))
  attr(matrix, "desc") <- desc
  class(matrix) <- c("GctMatrix", "matrix")
  return(matrix)
}

#' Retrieve feature (row) descriptions from a GctMatrix S3-object
#' @param gctMatrix A GctMatrix object
#' @param index Logical or integer index
#' @return Character vector, feature descriptions
#' @examples
#' m1 <- matrix(1:6, nrow=3, dimnames=list(sprintf("G%d", 1:3), sprintf("S%d", 1:2)))
#' gm1 <- GctMatrix(m1, desc=sprintf("Gene%d", 1:3))
#' gctDesc(gm1)
#' gctDesc(gm1, 1:2)
gctDesc <- function(gctMatrix, index) {
  res <- attr(gctMatrix, "desc")
  if(!missing(index))
    res <- res[index]
  return(res)
}

#' Print method for GctMatrix object
#' @param x A GctMatrix object
#' @param showAll Logical, whether all values should be printed
#' @param ... Paramters passed to the default method of \code{print}
#' @examples 
#' m1 <- matrix(1:6, nrow=3, dimnames=list(sprintf("G%d", 1:3), sprintf("S%d", 1:2)))
#' gm1 <- GctMatrix(m1, desc=sprintf("Gene%d", 1:3))
#' gm1
#' mBig <- matrix(round(rnorm(1000),3),
#'    nrow=100, dimnames=list(sprintf("G%d", 1:100), sprintf("S%d", 1:10)))
#' gmBig <- GctMatrix(mBig, desc=sprintf("Gene%d", 1:100))
#' gmBig
#' \dontrun{print(gmBig, showAll=TRUE)}
print.GctMatrix <- function(x, showAll=FALSE, ...) {
  cat(sprintf("A GctMatrix with %d features and %d samples.\n",
              nrow(x), ncol(x)))
  if(showAll) {
    NextMethod()
  } else {
    cat("-- Below are the first rows and columns, use `showAll=TRUE` to print all values\n")
    preview <- x[1:pmin(5, nrow(x)),
                 1:pmin(5, ncol(x)),
                 drop=FALSE]
    class(preview) <- "matrix"
    previewDf <- as.data.frame(preview)
    if(ncol(x)>ncol(preview))
      previewDf$`...` <- rep("...", nrow(previewDf))
    if(nrow(x)>nrow(preview)) {
      previewDf <- rbind(previewDf,
                         rep("...", ncol(previewDf)))
      rownames(previewDf)[nrow(previewDf)] <- "..."
    }
    ##attr(preview, "desc") <- NULL
    print.data.frame(previewDf)
    cat("First feature descriptions:", 
        paste(attr(preview, "desc"), collapse=","),
        "\n")
  }
  cat("-- Use `gctDesc` to fetch feature descriptions\n")
  cat("-- Use `rownames` and `colnames` to fetch row and column names\n")
  cat("-- All methods applicable to matrix can be used\n")
}

#' Subsetting for GctMatrix
#' @param x A GctMatrix object
#' @param i Index to subset rows
#' @param j Index to subset columns
#' @param ... Other parameters passed to matrix subsetting
#' 
#' @examples 
#' m1 <- matrix(1:6, nrow=3, dimnames=list(sprintf("G%d", 1:3), sprintf("S%d", 1:2)))
#' gm1 <- GctMatrix(m1, desc=sprintf("Gene%d", 1:3))
#' gm1[1:2,]
#' gm1[1:3,2:1]
#' gm1[1,]
#' gm1[,-1]
`[.GctMatrix` <- function(x, i, j, ...) {
  resMat <- NextMethod(`[`, drop=FALSE)
  attr(resMat, "desc") <- attr(x, "desc")[i]
  class(resMat) <- c("GctMatrix", "matrix")
  return(resMat)
}

#' Column bind (cbind) two GctMatrix objects
#' @param gctMatrix1 The first object
#' @param gctMatrix2 The second object
#' @param feature What happens if the set of the features in both objects differ? Either union or intersection is possible.
#' @param missingValue Missing values, \code{NA} or numeric values (such as 0) are accepted
#' 
#' @return A larger matrix, with gctMatrix1 on the left and gctMatrix2 on the right, with merged features and descriptions.
#' 
#' @examples
#' m1 <- matrix(1:6, nrow=3, dimnames=list(sprintf("G%d", 1:3), sprintf("S%d", 1:2)))
#' m2 <- matrix(c(9:7, 12:10), nrow=3, dimnames=list(sprintf("G%d", 3:1), sprintf("S%d", 3:4)))
#' gm1 <- GctMatrix(m1, desc=sprintf("Gene%d", 1:3))
#' gm2 <- GctMatrix(m2, desc=sprintf("Gene%d", 3:1))
#' gm1
#' gm2
#' gm12 <- cbindGct(gm1, gm2)
#' gm12
#' m3 <- matrix(13:18, nrow=3, dimnames=list(sprintf("G%d", 2:4), sprintf("S%d", 5:6)))
#' gm3 <- GctMatrix(m3, desc=sprintf("Gene%d", 2:4))
#' gm3
#' gm123Intersect <- cbindGct(gm12, gm3, feature="intersect")
#' print(gm123Intersect, showAll=TRUE)
#' gm123Union <- cbindGct(gm12, gm3, feature="union")
#' print(gm123Union, showAll=TRUE)
#' gm123UnionNA <- cbindGct(gm12, gm3, feature="union", missingValue = NA)
#' print(gm123UnionNA)
cbindGct <- function(gctMatrix1, gctMatrix2, 
                     feature=c("union", "intersection"),
                     missingValue=0) {
  feature <- match.arg(feature)
  stopifnot(is.numeric(missingValue) || is.na(missingValue))
  ## check uniqueness of column names
  concSamples <- c(colnames(gctMatrix1), colnames(gctMatrix2))
  if(any(duplicated(concSamples))) {
    warning(sprintf(paste("Duplicated sample names found (%d out of %d)!",
                          "They are made unique."),
                    sum(duplicated(concSamples)), length(concSamples)))
    concSamples <- make.unique(concSamples)
  }
  ## merge rownames and feature descriptions
  rn1 <- rownames(gctMatrix1)
  rn2 <- rownames(gctMatrix2)
  if(is.null(rn1) || is.null(rn2)) {
    stop("Row names of gctMatrix must be characters, not NULL.")
  }
  if(feature=="intersection") {
    rn <- intersect(rn1, rn2)
  } else if (feature=="union") {
    rn <- union(rn1, rn2)
  } else{
    stop("Should not be here - contact the developer!")
  }
  ## merge description
  rdescs <- unique(rbind(data.frame(Name=rn1, Desc=gctDesc(gctMatrix1),
                                    stringsAsFactors=FALSE),
                         data.frame(Name=rn2, Desc=gctDesc(gctMatrix2), 
                                    stringsAsFactors=FALSE)))
  desc <- ribiosUtils::matchColumn(rn, rdescs, "Name")$Desc
  
  ## fill the cbind-ed matrix
  mat <- matrix(missingValue, nrow=length(rn), ncol=length(concSamples),
                dimnames=list(rn, concSamples))
  rn1Ins <- intersect(rn, rn1)
  rn2Ins <- intersect(rn, rn2)
  mat[rn1Ins, colnames(gctMatrix1)] <- gctMatrix1[rn1Ins,,drop=FALSE]
  mat[rn2Ins, (ncol(gctMatrix1)+1):ncol(mat)] <- gctMatrix2[rn2Ins,,drop=FALSE]
  res <- GctMatrix(mat, desc)
  return(res)
}
