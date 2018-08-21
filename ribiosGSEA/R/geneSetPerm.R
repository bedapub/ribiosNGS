#' Test gene set enrichment by permutating gene labels of statistics
#'
#' @description
#' Test gene set enrichment by permutating gene labels of statistics
#'
#' @param stats Statistics
#' @param indList a list of integers, indicating indices of genes of gene sets (index starts from 1, following R's convention)
#' @param Nsim number of simulations
#'
#' @return A data frame containg mean statistic, gene set size, and p-values
#' @seealso \code{geneSetTest}, a R implementation in the limma package
#' @examples
#' set.seed(1887)
#' stats <- rnorm(1000)
#' gsList <- list(gs1=c(3,4,5), gs2=c(7,8,9))
#' geneSetPerm(stats, gsList, Nsim=9999)
#' gsList2 <- list(gs1=c(3,4,5), gs2=c(7,8,9), gs3=integer())
#' geneSetPerm(stats, gsList2, Nsim=9999)
#' gsList3 <- sample(1:1000, 200)
#' geneSetPerm(stats, gsList3, Nsim=9999)


geneSetPerm <- function(stats,
                        indList,
                        Nsim=9999) {
  stats <- as.numeric(stats)
  if(is.integer(indList) || is.numeric(indList)) {
    indList <- list(indList)
  }
  int.indList <- lapply(indList, as.integer)
  Nsim <- as.integer(Nsim)
  
  ## sanity check
  haltifnot(!any(is.na(stats)),
            msg="stats must not contain any NA values")
            

  hasNAind <- sapply(int.indList, function(x) any(is.na(x)))
  haltifnot(!any(hasNAind),
            msg=paste("Following gene sets has NAs",
              paste(which(hasNAind), collapse=",")))
            
  .Call("cpp_geneSetPerm", stats, int.indList, Nsim,
        PACKAGE="ribiosGSEA")
        
}
