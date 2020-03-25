#' Perform SVA analysis using VSN-transformed DGEList
#' @param dgeList An \code{DGEList} object
#' @param design Design matrix
#' @param nullModel Null model matrix
#' @param verbose Logical
#' @param offset If provided, it is passed to \code{pcaScores}.
#' 
#' In case no significant surrogate variables are detected, PCA analysis is 
#' applied to the vsn-transformed matrix.
#' 
#' @seealso \link[ribiosPlot]{pcaScores}
#' 
#' @return A list with following items
#' \itemize{
#'   \item{sva}{Results of \code{svaseq}}
#'   \item{vsnFit}{Fit object of vsn}
#'   \item{vsnMat}{Fitted matrix of vsn}
#'   \item{vsnBatchRemoved}{Fitted matrix of vsn, with surrogates' effect 
#'        removed}
#'   \item{vsnBatchRemovedPca}{PCA object derived from \code{vsnBatchRemoved}}
#'   \item{vsnBatchRemovedPcaScores}{PCA scores with annotations}
#'   \item{designWithSV}{Design matrix with surrogates variables appended
#'        if any}
#' }
#' 
#' @examples 
#' y1org <- matrix(rnbinom(4000, mu=5, size=2), ncol=8)
#' genes1 <- data.frame(GeneSymbol=sprintf("Gene%d", 1:nrow(y1org)))
#' y1 <- y1org
#' y1[30:120, 4:7] <- y1[30:120, 4:7]+9 ## mimicking batch effect
#' rownames(y1org) <- rownames(y1) <- rownames(genes1) <- 1:nrow(y1)
#' anno1 <- data.frame(treatment=gl(2,4, labels=c("ctrl", "tmt")),
#'     donor=factor(rep(c(1,2), 4)))
#' d1 <- DGEList(counts=y1, genes=genes1, samples=anno1)
#' d2 <- DGEList(counts=y1org, genes=genes1, samples=anno1)
#' 
#' design <- model.matrix(~treatment+donor, data=d1$samples)
#' nullModel <-  model.matrix(~donor, data=d1$samples)
#' d1VsnSvaRes <- vsnSVA(d1, design, nullModel)
#' d2VsnSvaRes <- vsnSVA(d2, design, nullModel)
vsnSVA <- function(dgeList,
                   design, nullModel, verbose=FALSE,
                   offset) {
  svaRes <- sva::svaseq(dgeList$counts,
                        mod = design,
                        mod0 = nullModel)
  dgeListFilter <- dgeList[rowSums(dgeList$counts) > 0, ]
  fit <- vsn::vsnMatrix(dgeList$counts, verbose = verbose)
  mat <- vsn::predict(fit, newdata = dgeList$counts, useDataInFit = TRUE)
  if(svaRes$n.sv!=0) {
    sv <- svaRes$sv
    colnames(sv) <- sprintf("sv%d", 1:ncol(sv))
    vsnBatchRemoved <- removeBatchEffect(mat, 
                                         covariates=sv, 
                                         design=design)
  } else {
    sv <- NULL
    vsnBatchRemoved <- mat
  }
  vsnBatchRemovedPca <- prcomp(t(vsnBatchRemoved))
  vbrPcascores <- ribiosPlot::pcaScores(vsnBatchRemovedPca,
                                        offset=offset)
  annoVbrPcascores <- cbind(vbrPcascores,
                            dgeList$samples)
  designWithSV <- cbind(design, sv)
  
  res <- list(sva=svaRes,
              vsnFit=fit,
              vsnMat=mat,
              vsnBatchRemoved=vsnBatchRemoved,
              vsnBatchRemovedPca=vsnBatchRemovedPca,
              vsnBatchRemovedPcaScores=annoVbrPcascores,
              designWithSV=designWithSV)
  return(res)
}

