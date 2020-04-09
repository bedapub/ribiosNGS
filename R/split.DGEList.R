#' Split a DGEList object by a factor of samples (default) or genes
#' 
#' @param x A \code{DGEList} object
#' @param f A factor vector. Other types will be coereced into factors.
#' @param drop Not used now
#' @param bySample Logical, if \code{TRUE}, the samples are split. Otherwise, 
#'   genes are split.
#' @param sampleDropLevels Logical, if \code{TRUE}, unused levels in factors 
#'   in the sample annotation are dropped
#' @param ... Not used so far.
#' 
#' @examples 
#' y1 <- matrix(rnbinom(1000, mu=5, size=2), ncol=4)
#' genes1 <- data.frame(GeneSymbol=sprintf("Gene%d", 1:nrow(y1)),
#'   GeneType=gl(5,50))
#' rownames(y1) <- rownames(genes1) <- 1:nrow(y1)
#' anno1 <- data.frame(treatment=gl(2,2, labels=c("ctrl", "tmt")),
#'     donor=factor(rep(c(1,2), each=2)))
#' d1 <- DGEList(counts=y1, genes=genes1, samples=anno1)
#' 
#' d1SampleSplit <- split(d1, d1$samples$donor)
#' d1GeneSplit <- split(d1, d1$genes$GeneType, bySample=FALSE)
#' 
#' @export 
split.DGEList <- function(x, f, drop=FALSE, bySample=TRUE, sampleDropLevels=TRUE, ...) {
  if(!is.factor(f))
    f <- as.factor(f)
  if(bySample) {
    res <- tapply(1:nrow(x$samples), f, function(ind) x[,ind])
  } else {
    res <- tapply(1:nrow(x$counts), f, function(ind) x[ind,])
  }
  if(sampleDropLevels & !is.null(x$samples)) {
    for(i in seq(along=res)) {
      for(j in 1:ncol(res[[i]]$samples)) {
         if(is.factor(res[[i]]$samples[,j])) {
            res[[i]]$samples[,j] <- droplevels(res[[i]]$samples[,j])
         }
      }
    }
  }
  resList <- new("DGEListList", .Data=res)
  names(resList) <- levels(f)
  return(resList)
}
