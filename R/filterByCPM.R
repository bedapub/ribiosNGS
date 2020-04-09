#' Filter lowly expressed genes by counts per million (CPM)
#' @param obj An object
#' @export
filterByCPM <- function(obj, ...) {
  UseMethod("filterByCPM")
}

#' Filter lowly expressed genes by CPM
#' 
#' @param obj A matrix
#' @param minCPM Numeric, the minimum CPM accepted as expressed in one sample
#' @param minCount Integer, how many samples must have CPM larger than \code{minCPM} to keep this gene?
#' @param ... Not used
#'
#' @return A logical vector of the same length as the row count of the matrix. \code{TRUE} means the gene is reasonably expressed, and \code{FALSE} means the gene is lowly expressed and should be filtered (removed)
#' 
#' @examples 
#' set.seed(1887)
#' mat <- rbind(matrix(rbinom(125, 5, 0.25), nrow=25), rep(0, 5))
#' filterByCPM(mat)
#' @export
filterByCPM.matrix <- function(obj, minCPM=1, minCount=1, ...) {
  cpmRes <- cpm(obj)
  filter <- apply(cpmRes, 1, function(x) sum(x>=minCPM)>=minCount)
  return(filter)
}

#' Filter lowly expressed genes by CPM in DGEList
#' 
#' @param obj A \code{DGEList} object
#' @param minCPM Numeric, the minimum CPM accepted as expressed in one sample
#' @param minCount Integer, how many samples must have CPM larger than 
#'     \code{minCPM} to keep this gene?
#' @parm lib.size Integers of library size, or \code{NULL}
#' @param ... Not used
#' 
#' @return Another \code{DGEList} object, with lowly expressed genes removed. 
#'   The original counts and gene annotation can be found in 
#'   \code{counts.unfiltered} and \code{genes.unfiltered} fields, respectively. 
#'   The logical vector of the filter is saved in the \code{cpmFilter} field.
#' 
#' @examples 
#' set.seed(1887)
#' mat <- rbind(matrix(rbinom(150, 5, 0.25), nrow=25), rep(0, 6))
#' d <- DGEList(mat, group=rep(1:3, each=2), 
#'              genes=data.frame(Gene=sprintf("Gene%d", 1:nrow(mat))))
#' df <- filterByCPM(d)
#' 
#' nrow(df$counts.unfiltered) ## 26
#' nrow(df$counts) ## 25
#' 
#' @importFrom edgeR DGEList
#' @export DGEList
#' @export
filterByCPM.DGEList <- function(obj, 
                                minCPM=1,
                                minCount=minGroupCount(obj),
				lib.size=NULL, ...) {
  y <- as.matrix(obj)
  genes <- obj$genes
  group <- obj$samples$group
  if (is.null(group)) 
    group <- rep_len(1L, ncol(y))
  group <- as.factor(group)
  if (is.null(lib.size)) 
    lib.size <- obj$samples$lib.size * obj$samples$norm.factors
  
  CPM <- cpm(y, lib.size = lib.size)
  filter <- rowSums(CPM >= minCPM) >= minCount
  res <- obj[filter,]
  res$cpmFilter <- filter
  res$counts.unfiltered <- y
  res$genes.unfiltered <- genes
  return(res)
}


#' Filter EdgeObj and remove lowly expressed genes
#' 
#' @param edgeObj An EdgeObject object
#' @param minCPM Minimal CPM value, see descriptions below
#' @param minCount Minimal count of samples in which the CPM value is no less
#' than \code{minCPM}
#' @param ... Not used
#' 
#' The filter is recommended by the authors of the \code{edgeR} package to
#' remove lowly expressed genes, since including them in differential gene
#' expression analysis will cause extreme differential expression fold-changes
#' of lowly and stochastically expressed genes, and increase false positive
#' rates.
#' 
#' The filter removes genes that are less expressed than 1 copy per million
#' reads (cpm) in at least \code{n} samples, where \code{n} equals the number
#' of samples in the smallest group of the design.
#' @examples
#' 
#' myFac <- gl(3,2)
#' set.seed(1234)
#' myMat <- matrix(rpois(1200,100), nrow=200, ncol=6)
#' myMat[1:3,] <- 0
#' myEdgeObj <- EdgeObject(myMat, 
#'                        DesignContrast(designMatrix=model.matrix(~myFac),
#'                         contrastMatrix=matrix(c(0,1,0), ncol=1), groups=myFac),
#'                         fData=data.frame(GeneSymbol=sprintf("Gene%d", 1:200)))
#' myFilteredEdgeObj <- filterByCPM(myEdgeObj)
#' dim(counts(myEdgeObj))
#' dim(counts(myFilteredEdgeObj))
#' ## show unfiltered count matrix
#' dim(counts(myFilteredEdgeObj, filter=FALSE))
#' 
#' @export
filterByCPM.EdgeObject <- function(obj,
                                   minCPM=1,
                                   minCount=minGroupCount(obj), ...) {
  cpmRes <- cpm(obj)
  filter <- apply(cpmRes, 1, function(x) sum(x>=minCPM)>=minCount)
  newDgeList <- obj@dgeList[filter,]
  newDgeList$counts.unfiltered <- obj@dgeList$counts
  newDgeList$genes.unfiltered <- obj@dgeList$genes
  obj@dgeList <- newDgeList
  return(obj)
}
