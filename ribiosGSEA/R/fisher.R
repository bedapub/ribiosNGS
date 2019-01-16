#' Core algorithm to perform Fisher's exact test on a gene set
#'
#' @param genes Character vector, a collection of genes of which over-representation of the gene set is tested
#' @param geneSetGenes Character vector, genes belonging to a gene set
#' @param universe Character vector, universe of genes
#' @param makeUniqueNonNA Logical, whether genes, geneSetGenes, and universe should be filtered to remove NA and made unique. The default is set to \code{TRUE}. When the uniqueness and absence of NA is ensured, this flag can be set to \code{FALSE} to accelerate the operation.
#' @param checkUniverse Logical, if \code{TRUE}, then genes that are in \code{genes} but are not in \code{universe} are appended to \code{universe}
#' @param useEASE Logical, whether to use the EASE method to report the p-value. 
#'
#' This function performs one-sided Fisher's exact test to test the over-representation of the genes given as \code{geneSetGenes} in the input \code{genes} list.
#' 
#' If \code{useEASE} is \code{TRUE}, one gene is penalized (removed) within \code{geneSetGenes} that are in \code{genes} and calculating the resulting Fisher exact probability for that category. The theoretical basis of the EASE score lies in the concept of jackknifing a probability. See Hosack \emph{et al.} for details.
#'
#' @return A list of three elements
#' \enumerate{
#'   \item p The p-value of one-sided (over-representation of the Fisher's test)
#'   \item gsEffSize Gene-set's effective size, namely number of genes that are in the universe
#'   \item hits Character vector, genes that are found in the gene sets
#' }
#' @references 
#' \describe{
#'   \item{Hosack \emph{et al.}}{Hosack, Douglas A., Glynn Dennis, Brad T. Sherman, H. Clifford Lane, and Richard A. Lempicki. Identifying Biological Themes within Lists of Genes with EASE. Genome Biology 4 (2003): R70. \url{https://doi.org/10.1186/gb-2003-4-10-r70}}
#' }
#' @examples
#' myGenes <- LETTERS[1:3]
#' myGeneSet1 <- LETTERS[1:6]
#' myGeneSet2 <- LETTERS[4:7]
#' myUniverse <- LETTERS
#' gsFisherTestCore(myGenes, myGeneSet1, myUniverse)
#' gsFisherTestCore(myGenes, myGeneSet2, myUniverse)
#' 
#' ## use EASE for conservative estimating
#' gsFisherTestCore(myGenes, myGeneSet1, myUniverse, useEASE=FALSE)
#' gsFisherTestCore(myGenes, myGeneSet1, myUniverse, useEASE=TRUE)
#' 
#' ## checkUniverse will make sure that \code{univese} contains all element in \code{genes}
#' gsFisherTestCore(c("OutOfUniverse", myGenes), myGeneSet1, myUniverse, checkUniverse=FALSE)
#' gsFisherTestCore(c("OutOfUniverse", myGenes), myGeneSet1, myUniverse, checkUniverse=TRUE)
gsFisherTestCore <- function(genes, geneSetGenes, universe, 
                             makeUniqueNonNA=TRUE,
                             checkUniverse=TRUE,
                             useEASE=FALSE) {
  if(makeUniqueNonNA) {
    genes <- uniqueNonNA(genes)
    geneSetGenes <- uniqueNonNA(geneSetGenes)
    universe <- uniqueNonNA(universe)
  }
  if(checkUniverse) {
    geneNotInUniverse <- !genes %in% universe
    if(any(geneNotInUniverse)) {
      universe <- union(universe, geneNotInUniverse)
    }
  }
  
  is.pos.geneset <- genes %in% geneSetGenes
  
  geneSetGenes <- intersect(geneSetGenes, universe)
  
  pos.geneset <- sum(is.pos.geneset)
  pos.nonGeneset <- length(genes)-pos.geneset
  neg.geneset <- length(geneSetGenes)-pos.geneset
  neg.nonGeneset <- length(universe)-pos.geneset-pos.nonGeneset-neg.geneset
  
  if(useEASE) {
    pos.geneset <- pos.geneset - 1
  }
  
  mat <- matrix(c(pos.geneset, pos.nonGeneset, neg.geneset, neg.nonGeneset),
                byrow=TRUE, nrow=2)
  fisher.p <- fisher.test(mat, alternative="greater")$p.value
  hits <- genes[is.pos.geneset]
  
  res <- list(p=fisher.p,
             gsEffSize=length(geneSetGenes),
             hits=hits)
  return(res)
}

#' Core algorithm to perform Fisher's exact test on a list of gene set
#'
#' @param genes Character vector, a collection of genes of which over-representation of the gene set is tested
#' @param geneSetGenesList A list of character vector, genes belonging to each gene set
#' @param universe Character vector, universe of genes
#' @param makeUniqueNonNA Logical, whether genes, geneSetGenes, and universe should be filtered to remove NA and made unique. The default is set to \code{TRUE}. When the uniqueness and absence of NA is ensured, this flag can be set to \code{FALSE} to accelerate the operation.
#' @param checkUniverse Logical, if \code{TRUE}, then genes that are in \code{genes} but are not in \code{universe} are appended to \code{universe}
#' @param useEASE Logical, whether to use the EASE method to report the p-value. 
#'
#' This function performs one-sided Fisher's exact test to test the over-representation of the genes given as \code{geneSetGenes} in the input \code{genes} list.
#' 
#' If \code{useEASE} is \code{TRUE}, one gene is penalized (removed) within \code{geneSetGenes} that are in \code{genes} and calculating the resulting Fisher exact probability for that category. The theoretical basis of the EASE score lies in the concept of jackknifing a probability. See Hosack \emph{et al.} for details.
#'
#' @return A list of lists, of the same length as the input geneSetGenesList, each list consisting of three elements
#' \enumerate{
#'   \item p The p-value of one-sided (over-representation of the Fisher's test)
#'   \item gsEffSize Gene-set's effective size, namely number of genes that are in the universe
#'   \item hits Character vector, genes that are found in the gene sets
#' }
#' @references 
#' \describe{
#'   \item{Hosack \emph{et al.}}{Hosack, Douglas A., Glynn Dennis, Brad T. Sherman, H. Clifford Lane, and Richard A. Lempicki. Identifying Biological Themes within Lists of Genes with EASE. Genome Biology 4 (2003): R70. \url{https://doi.org/10.1186/gb-2003-4-10-r70}}
#' }
#' 
#' @seealso \code{\link{gsFisherTestCore}}
#' @examples
#' myGenes <- LETTERS[1:3]
#' myGeneSet1 <- LETTERS[1:6]
#' myGeneSet2 <- LETTERS[4:7]
#' myUniverse <- LETTERS
#' gsListFisherTestCore(myGenes, list(myGeneSet1, myGeneSet2), myUniverse)
gsListFisherTestCore <- function(genes, geneSetGenesList, universe, 
                             makeUniqueNonNA=TRUE,
                             checkUniverse=TRUE,
                             useEASE=FALSE) {
  if(makeUniqueNonNA) {
    genes <- uniqueNonNA(genes)
    geneSetGenes <- lapply(geneSetGenesList, uniqueNonNA)
    universe <- uniqueNonNA(universe)
  }
  if(checkUniverse) {
    geneNotInUniverse <- !genes %in% universe
    if(any(geneNotInUniverse)) {
      universe <- union(universe, geneNotInUniverse)
    }
  }
  
  res <- lapply(geneSetGenesList, function(geneSetGenes) {
    res <- gsFisherTestCore(genes=genes,
                            geneSetGenes=geneSetGenes,
                            universe=universe,
                            makeUniqueNonNA=FALSE,
                            checkUniverse=FALSE,
                            useEASE=useEASE)
  })
  names(res) <- names(geneSetGenesList)
  return(res)
}


#' Perform Fisher's exact test on a gene set
#'
#' @param genes a collection of genes of which over-representation of the gene set is tested
#' @param geneSetGenes genes belonging to a gene set
#' @param universe universe of genes
#' @param gsName gene set name, can be left missing
#' @param gsCategory gene set category name, can be left missing
#' @param makeUniqueNonNA Logical, whether genes, geneSetGenes, and universe should be filtered to remove NA and made unique. The default is set to \code{TRUE}. When the uniqueness and absence of NA is ensured, this flag can be set to \code{FALSE} to accelerate the operation.
#' @param checkUniverse Logical, if \code{TRUE}, then genes that are in \code{genes} but are not in \code{universe} are appended to \code{universe}
#' @param useEASE Logical, whether to use the EASE method to report the p-value. 
#'
#' This function performs one-sided Fisher's exact test to test the over-representation of gene set genes in the input gene list.
#' 
#' If \code{useEASE} is \code{TRUE}, one gene is penalized (removed) within \code{geneSetGenes} that are in \code{genes} and calculating the resulting Fisher exact probability for that category. The theoretical basis of the EASE score lies in the concept of jackknifing a probability. See Hosack \emph{et al.} for details.
#'
#' @references 
#' \describe{
#'   \item{Hosack \emph{et al.}}{Hosack, Douglas A., Glynn Dennis, Brad T. Sherman, H. Clifford Lane, and Richard A. Lempicki. Identifying Biological Themes within Lists of Genes with EASE. Genome Biology 4 (2003): R70. \url{https://doi.org/10.1186/gb-2003-4-10-r70}}
#' }
#' 
#' @note Duplicated items in genes, genesets' genes, and the universe are per default removed
#' 
#' @examples
#' myGenes <- LETTERS[1:3]
#' myGeneSet1 <- LETTERS[1:6]
#' myGeneSet2 <- LETTERS[4:7]
#' myUniverse <- LETTERS
#' fisherTest(myGenes, myGeneSet1, myUniverse)
#' fisherTest(myGenes, myGeneSet2, myUniverse)
#' fisherTest(myGenes, myGeneSet1, myUniverse, gsName="My gene set1", gsCategory="Letters")
#'
#' ## note that duplicated items are removed by default
#' resWoRp <- fisherTest(rep(myGenes,2), myGeneSet1, myUniverse)
#' resWithRp <- fisherTest(rep(myGenes,2), myGeneSet1, rep(myUniverse,2))
#' identical(resWoRp, resWithRp)
#' 
#' resWithRpNoUnique <- fisherTest(rep(myGenes,2), myGeneSet1, rep(myUniverse,2), makeUniqueNonNA=FALSE)
#' identical(resWoRp, resWithRpNoUnique)
setMethod("fisherTest", c("character", "character", "character"),
          function(genes, genesets, universe, gsName, gsCategory,
                   makeUniqueNonNA=TRUE, 
                   checkUniverse=TRUE,
                   useEASE=FALSE) {
              if(missing(gsName))
                  gsName <- as.character(NA)
              if(missing(gsCategory))
                  gsCategory <- as.character(NA)
              coreRes <- gsFisherTestCore(genes = genes, 
                           geneSetGenes = genesets,
                           universe = universe,
                           makeUniqueNonNA = makeUniqueNonNA,
                           checkUniverse = checkUniverse,
                           useEASE = useEASE)
              new("FisherResult",
                  gsCategory=gsCategory,
                  gsName=gsName,
                  gsEffSize=coreRes$gsEffSize,
                  hits=coreRes$hits,
                  p=coreRes$p,
                  fdr=coreRes$p)
          })

#' Perform Fisher's exact test on a GeneSet object
#'
#' @param genes a collection of genes of which over-representation of the gene set is tested
#' @param geneSetGenes A GeneSet object
#' @param universe universe of genes
#' @param makeUniqueNonNA Logical, whether genes and universe should be filtered to remove NA and made unique. The default is set to \code{TRUE}. When the uniqueness and absence of NA is ensured, this flag can be set to \code{FALSE} to accelerate the operation.
#' @param checkUniverse Logical, if \code{TRUE}, then genes that are in \code{genes} but are not in \code{universe} are appended to \code{universe}
#' @param useEASE Logical, whether to use the EASE method to report the p-value. 
#'
#' This function performs one-sided Fisher's exact test to test the over-representation of gene set genes in the input gene list.
#'
#' @examples
#' myGenes <- LETTERS[1:3]
#' myS4GeneSet1 <- new("GeneSet", category="My category 1", name="GeneSet1", genes=LETTERS[1:6])
#' myS4GeneSet2 <- new("GeneSet", category="My category 2", name="GeneSet1", genes=LETTERS[2:7])
#' myUniverse <- LETTERS
#' fisherTest(myGenes, myS4GeneSet1, myUniverse)
#' fisherTest(myGenes, myS4GeneSet2, myUniverse)
setMethod("fisherTest", c("character", "GeneSet", "character"),
          function(genes, genesets, universe,
                   makeUniqueNonNA=TRUE, 
                   checkUniverse=TRUE,
                   useEASE=FALSE) {
            if(makeUniqueNonNA) {
              genes <- uniqueNonNA(genes)
              universe <- uniqueNonNA(universe)
            }
            ## gsGenes(genesets) are garanteed to be unique and non-NA
            ## therefore fisherTest now takes makeUniqueNonNA=FALSE
            fisherTest(genes=genes, genesets=gsGenes(genesets),
                       universe=universe,
                       gsName=gsName(genesets), 
                       gsCategory=gsCategory(genesets),
                       makeUniqueNonNA=FALSE,
                       checkUniverse=checkUniverse,
                       useEASE=useEASE)
          })


#' Perform Fisher's exact test on a GeneSets object
#' 
#' @param genes character strings of gene list to be tested
#' @param genesets An GeneSets object
#' @param universe Universe (background) gene list
#' @param makeUniqueNonNA Logical, whether genes and universe should be filtered to remove NA and made unique. The default is set to \code{TRUE}. When the uniqueness and absence of NA is ensured, this flag can be set to \code{FALSE} to accelerate the operation.
#' @param checkUniverse Logical, if \code{TRUE}, then genes that are in \code{genes} but are not in \code{universe} are appended to \code{universe}
#' @param useEASE Logical, whether to use the EASE method to report the p-value. 
#'
#'#' @return A \code{data.frame} containing Fisher's exact test results of all gene-sets, in the same order as the input gene-sets, with following columns:
#' \enumerate{
#'   \item GeneSetCategory
#'   \item GeneSetName
#'   \item GeneSetEffectiveSize, the count of genes in the gene-set that are found in the universe
#'   \item HitCount, the count of genes in the \code{genes} input that are in the gene-set
#'   \item Hits, a comma-delimited character string of htis
#'   \item PValue
#'   \item FDR, PValue adjusted by the Benjamini-Hochberg method. If more than one gene-set categories are provided, the FDR correction is performed per category
#' }
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
#' 
#' ## Note that the multiple adjustment is done by categories
#' gs5 <- new("GeneSet", name="GeneSet5", genes=LETTERS[1:4])
#' gs6 <- new("GeneSet", name="GeneSet6", genes=LETTERS[5:8])
#' gs7 <- new("GeneSet", name="GeneSet7", genes=LETTERS[seq(2,8,2)])
#' gs8 <- new("GeneSet",  name="GeneSet8", genes=LETTERS[seq(1,7,2)])
#' gss2 <- GeneSets(list(gs5, gs6, gs7, gs8))
#' myFisherRes2 <- fisherTest(myInput, gss2, myUniverse)
#' 
#' stopifnot(identical(myFisherRes$PValue, myFisherRes2$PValue))
#' stopifnot(!identical(p.adjust(myFisherRes$PValue, "fdr"), myFisherRes$FDR))
#' stopifnot(identical(p.adjust(myFisherRes2$PValue, "fdr"), myFisherRes2$FDR))
setMethod("fisherTest", 
          c("character", "GeneSets", "character"),
          function(genes, genesets, universe,
                   makeUniqueNonNA = TRUE,
                   checkUniverse = TRUE, 
                   useEASE = FALSE) {
            if(makeUniqueNonNA) {
              genes <- uniqueNonNA(genes)
              universe <- uniqueNonNA(universe)
            }
            if(checkUniverse) {
              geneNotInUniverse <- !genes %in% universe
              if(any(geneNotInUniverse)) {
                universe <- union(universe, geneNotInUniverse)
              }
            }
            res <- lapply(genesets, function(x) {
              gsFisherTestCore(genes = genes, 
                                          geneSetGenes = gsGenes(x),
                                          universe = universe,
                                          makeUniqueNonNA = makeUniqueNonNA,
                                          checkUniverse = checkUniverse,
                                          useEASE = useEASE)
            })
            res <- data.frame(GeneSetCategory=gsCategory(genesets),
                              GeneSetName=gsName(genesets),
                              GeneSetEffectiveSize=sapply(res, function(x) x$gsEffSize),
                              HitCount=sapply(res, function(x) length(x$hits)),
                              Hits=sapply(res, function(x) paste(x$hits, collapse=",",sep="")),
                              PValue=sapply(res, function(x) x$p))
            if(all(is.na(res$GeneSetCategory))) {
              res$FDR <- p.adjust(res$PValue, method="fdr")
            } else {
              res$FDR <- ave(res$PValue, res$GeneSetCategory, 
                           FUN=function(x) p.adjust(x, "fdr"))
            }
            return(res)
          })

#' Perform Fisher's exact test on a GmtList object
#' @param genes character strings of gene list to be tested
#' @param genesets An GmtList object
#' @param universe Universe (background) gene list
#' @param makeUniqueNonNA Logical, whether genes and universe should be filtered to remove NA and made unique. The default is set to \code{TRUE}. When the uniqueness and absence of NA is ensured, this flag can be set to \code{FALSE} to accelerate the operation.
#' @param checkUniverse Logical, if \code{TRUE}, then genes that are in \code{genes} but are not in \code{universe} are appended to \code{universe}
#' @param useEASE Logical, whether to use the EASE method to report the p-value. 
#' 
#' @return A data.frame
setMethod("fisherTest", 
          c("character", "GmtList", "character"),
          function(genes, genesets, universe,
                   gsCategory,
                   makeUniqueNonNA = TRUE,
                   checkUniverse = TRUE, useEASE = FALSE) {
            if(makeUniqueNonNA) {
              genes <- uniqueNonNA(genes)
              universe <- uniqueNonNA(universe)
            }
            if(checkUniverse) {
              geneNotInUniverse <- !genes %in% universe
              if(any(geneNotInUniverse)) {
                universe <- union(universe, geneNotInUniverse)
              }
            }
            if(missing(gsCategory) || is.null(gsCategory)) {
              gsCategory <- NA
            } else {
              haltifnot(length(gsCategory) == length(genesets),
                        msg=sprintf("Length of category (%d) does not match length of the gene-sets (%d)", 
                                   length(gsCategory), length(genesets)))
            }
            res <- lapply(genesets, function(x) {
              gsFisherTestCore(genes = genes, 
                               geneSetGenes = x$genes,
                               universe = universe,
                               makeUniqueNonNA = makeUniqueNonNA,
                               checkUniverse = checkUniverse,
                               useEASE = useEASE)
            })

            res <- data.frame(GeneSetCategory=gsCategory,
                              GeneSetName=sapply(genesets, function(x) x$name),
                              GeneSetEffectiveSize=sapply(res, function(x) x$gsEffSize),
                              HitCount=sapply(res, function(x) length(x$hits)),
                              Hits=sapply(res, function(x) paste(x$hits, collapse=",",sep="")),
                              PValue=sapply(res, function(x) x$p))

            if(all(is.na(gsCategory))) {
              res$FDR <- p.adjust(res$PValue, method="fdr")
            } else {
              res$FDR <- ave(res$PValue, gsCategory, 
                             FUN=function(x) p.adjust(x, "fdr"))
            }
            return(res)
          })

