#' Perform Fisher's exact test on a gene set
#'
#' @param genes a collection of genes of which over-representation of the gene set is tested
#' @param geneSetGenes genes belonging to a gene set
#' @param universe universe of genes
#' @param gsName gene set name
#' @param gsCategory gene set category name
#'
#' This function performs one-sided Fisher's exact test to test the over-representation of gene set genes in the input gene list. Note that all three parameters must not contain duplicates! The S4-class methods ensure this by checking the input parameters.
#' 
gsFisherTest <- function(genes, geneSetGenes, universe, gsName, gsCategory) {
    if(missing(gsName)) gsName <- NA
    if(missing(gsCategory)) gsCategory <- NA
    is.pos.geneset <- genes %in% geneSetGenes
    pos.geneset <- sum(is.pos.geneset)
    pos.nonGeneset <- length(genes)-pos.geneset
    neg.geneset <- length(geneSetGenes)-pos.geneset
    neg.nonGeneset <- length(universe)-pos.geneset-pos.nonGeneset-neg.geneset
    mat <- matrix(c(pos.geneset, pos.nonGeneset, neg.geneset, neg.nonGeneset),
                  byrow=TRUE, nrow=2)
    fisher.p <- fisher.test(mat, alternative="greater")$p.value
    hits <- genes[is.pos.geneset]
    fr <- new("FisherResult",
              gsCategory=as.character(gsCategory),
              gsName=as.character(gsName),
              hits=hits,
              gsSize=length(geneSetGenes),
              p=fisher.p)
    return(fr)
}

setMethod("fisherTest", c("character", "character", "character", "ANY", "ANY"),
          function(genes, genesets, universe, gsName, gsCategory) {
              if(missing(gsName))
                  gsName <- as.character(NA)
              if(missing(gsCategory))
                  gsCategory <- as.character(NA)
              genes <- unique(genes)
              universe <- unique(universe)
              gsFisherTest(genes=genes, geneSetGenes=genesets,
                           universe=universe,
                           gsName=gsName, gsCategory=gsCategory)
          })
setMethod("fisherTest", c("character", "GeneSet", "character", "missing", "missing"),
          function(genes, genesets, universe) {
              fisherTest(genes=genes, genesets=gsGenes(genesets),
                         universe=universe,
                         gsName=gsName(genesets), gsCategory=gsCategory(genesets))
          })
setMethod("fisherTest", c("character", "GeneSets", "character", "missing", "missing"),
          function(genes, genesets, universe) {
              genes <- unique(genes)
              universe <- unique(universe)
              res <- lapply(genesets, function(x) fisherTest(genes, x, universe=universe))
              names(res) <- NULL
                  ps <- sapply(res, function(x) x@p)
              fdr <- unname(p.adjust(ps, "fdr"))
              for(i in seq(along=res)) {
                  res[[i]]@fdr <- fdr[i]
              }
              fr <- new("FisherResultList", res)
              fr@input <- genes
              fr@universe <- universe
              return(fr)
          })
