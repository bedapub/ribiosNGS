#' Calculate TF-IDF using a input matrix with terms in rows and documents in columns
#' @param tdMat A term-document matrix, terms in rows, documents in columns, and counts as integers (or logical values) in cells
#' @param tfVariant Variant of term frequency. See details below.
#' @param idfVariant Variant of inverse document frequency. See details below.
#' @param idfAddOne Logical, whether one should be added to both numerator and denominator to calculate IDF. See details below.
#' 
#' @details
#' \code{tfVariant} accepts following options:
#' \describe{
#'   \item{raw}{The input matrix is used as it is.}
#'   \item{binary}{The input matrix is transformed into logical values.}
#'   \item{frequency}{Term frequency per document is calculated from the input matrix.}
#'   \item{log}{Transformation \code{log(1+tfMat)} }
#'   \item{doubleNorm0.5}{Double normalisation 0.5}
#' }
#' 
#' \code{idfVariant} accepts following options:
#' \describe{
#'   \item{raw}{\code{log(N/Nt)}}
#'   \item{smooth}{\code{log(1+N/Nt)}}
#'   \item{probabilistic}{\code{log((N-nt)/nt)}}
#' }
#' , where \code{N} represents the total number of documents in the corpus, and \code{nt} is the number of documents where the term \code{t} appears. If \code{idfAddOne} is set \code{TRUE}, both numbers with addition of 1 to prevent division-by-zero.
#' 
#' See \href{https://en.wikipedia.org/wiki/Tf%E2%80%93idf}{Wikipedia} for more information.
#' 
#' @examples 
#' tiExample <- matrix(c(1,1,1,1,1,
#' 1,1,0,0,0,
#' 1,0,0,0,0,
#' 0,1,0,0,0,
#' 0,0,0,1,0,
#' 1,0,1,0,1,
#' 0,0,0,0,1), ncol=5, byrow=TRUE)
#' colnames(tiExample) <- sprintf("D%d", 1:ncol(tiExample))
#' rownames(tiExample) <- sprintf("t%d", 1:nrow(tiExample))
#' tiRes <- tfidf(tiExample)
#' 
#' @export
tfidf <- function(tdMat, 
                  tfVariant=c("raw", "binary", "frequency", "log", "doubleNorm0.5"),
                  idfVariant=c("raw", "smooth", "probabilistic"),
                  idfAddOne=TRUE) {
  if(storage.mode(tdMat) == "logical") {
    storage.mode(tdMat) <- "integer"
  }
  tdMat[is.na(tdMat)] <- 0L
  tfVariant <- match.arg(tfVariant)

  if(tfVariant=="raw") {
    tf <- tdMat
  } else if (tfVariant=="binary") {
    tf <- tdMat>0 + 0
  } else if (tfVariant=="frequency"){ 
    tf <- t(t(tdMat)/colSums(tdMat))
  } else if (tfVariant=="log") {
    tf <- log(1+tdMat)
  } else if (tfVariant=="doubleNorm0.5") {
    rowMaxs <- apply(tdMat, 2, max)
    tf <- 0.5 + 0.5*t(t(tdMat)/rowMaxs)
  } else {
    stop("Should not be here")
  }
 
  idfVariant <- match.arg(idfVariant) 
  nt <- apply(tdMat, 1, function(x) sum(x>0))
  nc <- ncol(tdMat)
  if(idfAddOne) {
    nt <- nt+1
    nc <- nc+1
  }
  if(idfVariant=="raw") {
    idf <- log(nc/nt)
  } else if (idfVariant=="smooth") {
    idf <- log(1+nc/nt)
  } else if (idfVariant=="probabilistic") {
    idf <- log((nc-nt)/nt)
  } else {
    stop("Should not be here")
  }
  res <- tf * idf
  return(res)
}

