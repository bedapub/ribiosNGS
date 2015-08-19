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
#' @examples
#' myGenes <- LETTERS[1:3]
#' myGeneSet1 <- LETTERS[1:6]
#' myGeneSet2 <- LETTERS[4:7]
#' myUniverse <- LETTERS
#' gsFisherTest(myGenes, myGeneSet1, myUniverse)
#' gsFisherTest(myGenes, myGeneSet2, myUniverse)
#' gsFisherTest(myGenes, myGeneSet1, myUniverse, gsName="My gene set1", gsCategory="Letters")
gsFisherTest <- function(genes, geneSetGenes, universe, gsName, gsCategory) {
    if(missing(gsName)) gsName <- NA
    if(missing(gsCategory)) gsCategory <- NA

    is.pos.geneset <- genes %in% geneSetGenes

    geneSetGenes <- intersect(geneSetGenes, universe)
    
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
              gsEffSize=length(geneSetGenes),
              hits=hits,
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

#' fisherTest
#'
#' @param genes character strings of gene list to be tested
#' @param genesets An GeneSets object
#' @param universe Universe (background) gene list
#'
#' @examples
#' gs1 <- new("GeneSet", category="A", name="GeneSet1", genes=LETTERS[1:4])
#' gs2 <- new("GeneSet", category="A", name="GeneSet2", genes=LETTERS[5:8])
#' gs3 <- new("GeneSet", category="A", name="GeneSet3", genes=LETTERS[seq(2,8,2)])
#' gs4 <- new("GeneSet", category="B", name="GeneSet4", genes=LETTERS[seq(1,7,2)])
#' gss <- GeneSets(list(gs1, gs2, gs3, gs4))
#' myInput <- LETTERS[2:6]
#' myUniverse <- LETTERS
#' myFisherRes <- fisherTest(myInput, gss, myUniverse)
#' ## Note that the multiple adjustment is done by categories
#' gs5 <- new("GeneSet", name="GeneSet5", genes=LETTERS[1:4])
#' gs6 <- new("GeneSet", name="GeneSet6", genes=LETTERS[5:8])
#' gs7 <- new("GeneSet", name="GeneSet7", genes=LETTERS[seq(2,8,2)])
#' gs8 <- new("GeneSet",  name="GeneSet8", genes=LETTERS[seq(1,7,2)])
#' gss2 <- GeneSets(list(gs5, gs6, gs7, gs8))
#' myFisherRes2 <- fisherTest(myInput, gss2, myUniverse)#'
#' stopifnot(!identical(p.adjust(pValue(myFisherRes), "fdr"), fdrValue(myFisherRes)))
#' stopifnot(identical(p.adjust(pValue(myFisherRes2), "fdr"), fdrValue(myFisherRes2)))
#' 
setMethod("fisherTest", c("character", "GeneSets", "character", "missing", "missing"),
          function(genes, genesets, universe) {
              genes <- unique(genes)
              universe <- unique(universe)
              if(any(!genes %in% universe)) {
                  warning("Incomplete universe: following genes are added",
                          paste(setdiff(genes, universe), collapse=","))
                  universe <- union(universe, genes)
              }
              res <- lapply(genesets, function(x) fisherTest(genes, x, universe=universe))
              names(res) <- names(genesets)
              fr <- new("FisherResultList", res)
              fr@input <- genes
              fr@universe <- universe
              fr <- estimateFdr(fr)
              return(fr)
          })
