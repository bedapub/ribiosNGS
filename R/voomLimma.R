#' Perform the voom+limma procedure
#' @param dgeList A DGEList object, it should be ideally already filtered 
#' @param design The design matrix
#' @param contrasts The contrast matrix
#' @param normalize.method Character string, passed to \code{voom}, keep it \code{none} unless you are sure
#' @param block Blocking factor, passed to \code{voom}
#' @param correlation Correlation between duplicates, passed to \code{voom}
#' @param weights Weights, passed to \code{voom}
#' @param plot Logical, whether the variance-mean relationship should be ploted
#' @param ... Passed to \code{\link[limma]{eBayes}}
#' 
#' @return \code{MArrayLM} object returned by \code{\link[limma]{eBayes}}, with voom object in the \code{voom} element of the list
#' 
#' @examples
#' y <- matrix(rnbinom(10000,mu=5,size=2),ncol=4)
#' d <- edgeR::DGEList(counts=y, group=rep(1:2,each=2))
#' d <- edgeR::calcNormFactors(d)
#' design <- model.matrix(~gl(2,2))
#' colnames(design) <- c("baseline", "treatment")
#' contrasts <- limma::makeContrasts("treatment", levels=design)
#' dvl <- voomLimma(d, design=design, contrasts=contrasts)
#' 
#' @importFrom limma voom lmFit contrasts.fit eBayes
#' @importFrom edgeR calcNormFactors
#' @export
voomLimma <- function(dgeList, design, contrasts,
                      normalize.method="none",
                      block=NULL, correlation=NULL, weights=NULL, plot=FALSE, ...) {
  dgeList <- edgeR::calcNormFactors(dgeList)
  voomObj <- limma::voom(dgeList, design=design,
                  normalize.method=normalize.method,
                  block=block, correlation=correlation, weights=weights, plot=plot)
  fit <- limma::lmFit(voomObj, design=design,
               block=block,
               correlation=correlation)
  fit2 <- limma::contrasts.fit(fit, contrasts=contrasts)
  fit2 <- limma::eBayes(fit2, ...)
  fit2$voom <- voomObj
  return(fit2)
}
