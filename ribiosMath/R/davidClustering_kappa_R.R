#' Cluster rows of a Kappa-statistic-matrix by the hierarhical fuzzy multi-linkage partitioning method proposed by DAVID
#'
#' The function implements the Hierarhical fuzzy multi-linkage partitioning method used in the DAVID Bioinformatics tool.
#'
#' @param kappaMatrix A numeric matrix of Kappa statistics, which is likely returned by \code{\link{rowKappa}} or \code{\link{colKappa}}.
#' @param kappaThr Numeric, the threshold of the Kappa statistic, which is used to select initial seeds. Default value: 0.35, as recommended by the authors of the original study based on their experiences.
#' @param initialGroupMembership Integer, the number of minimal members in initial groups. Default value: 3.
#' @param multiLinkageThr Numeric, the minimal linkage between two groups to be merged. Default value: 0.5.
#' @param removeRedundant Logical, whether redundant initial groups should be removed before clustering. Used for debugging. Setting as \code{TRUE} accelerates the iterative clustring process. 
#' @param debug Logical, whether seed information is printed for debugging purposes.
#' 
#' @author Jitao David Zhang <jitao_david.zhang@roche.com>
#'
#' @note
#' The function has only been tested in a few anecdotal examples. Cautions and more systematic tests are required before it is applied to critical datasets.
#'
#' @references
#' \itemize{
#' \item{Huang *et al.* The DAVID Gene Functional Classification Tool: a novel biological module-centric algorithm to functionally analyze large gene lists. Genome Biology, 2007}
#' \item{Additional file of the manuscript available at \url{https://david.ncifcrf.gov/helps/2D_Introduction_files/additional_file_13.doc}}
#' }
#'
#' @examples 
#' synData <- matrix(c(rep(c(rep(1, 10), rep(0, 5)), 3),
#' rep(0, 4), rep(1, 7), rep(0,4),
#' rep(c(rep(0,5), rep(1,10)), 3),
#' rep(c(rep(0,3), 1), 4)[-16]), ncol=15, byrow=TRUE)
#' rownames(synData) <- sprintf("Gene %s", letters[1:8])
#' colnames(synData) <- sprintf("t%d", 1:15)
#' synRowKappa <- rowKappa(synData)
#' synColKappa <- colKappa(synData)
#' ribiosMath:::davidClustering_kappa_R(synRowKappa, removeRedundant = TRUE, debug=FALSE)
#' ribiosMath:::davidClustering_kappa_R(synColKappa, removeRedundant = TRUE, debug=FALSE)
#' 
davidClustering_kappa_R <- function(kappaMatrix, 
                            kappaThr = 0.35,
                            initialGroupMembership=3,
                            multiLinkageThr=0.5,
                            ## the following parameters are used for debugging
                            removeRedundant=TRUE,
                            debug=FALSE) {
  matBin <- kappaMatrix >= kappaThr
  ## note that order in seeds is important: the first element is the initial seed
  seeds <- sapply(1:nrow(matBin), function(i) {
    x <- matBin[i,]
    members <- which(x)
    c(i, setdiff(members, i))
  })
  if(removeRedundant) {
    ordSeeds <- lapply(seeds, sort)
    dupSeeds <- duplicated(ordSeeds)
    seeds <- seeds[!dupSeeds]
  }
  seeds <- seeds[sapply(seeds, length)>=initialGroupMembership]
  if(length(seeds)==0)
    return(list())
  
  isMultiLinkage <- sapply(seeds, function(x) {
    nonInitialSeed <- x[-1]
    subKappa <- kappaMatrix[nonInitialSeed, nonInitialSeed]
    pwKappa <- subKappa[lower.tri(subKappa, diag=FALSE)]
    isLinked <- mean(pwKappa >= kappaThr)>=0.5
    return(isLinked)
  })
  seeds <- seeds[isMultiLinkage]
  
  ## iteratively merge seeds until no two seeds share majority (multiLinkageThr) of members
  if(length(seeds)>1) {
  lastSeedCount <- length(seeds)
  newSeedCount <- NA
  while(is.na(newSeedCount) || lastSeedCount!=newSeedCount) {
    if(debug) {
      print(seeds)
    }
    lastSeedCount <- length(seeds)
    changed <- FALSE
    for(i in seq(1, lastSeedCount-1)) {
      for(j in seq(i+1, lastSeedCount)) {
        if(j>lastSeedCount)
          break;
        seedsi <- seeds[[i]]
        seedsj <- seeds[[j]]
        commonLen <- length(intersect(seedsi, seedsj))
        total <- union(seedsi, seedsj)
        totalLen <- length(total)
        linkage <- commonLen/totalLen
        if(linkage >= multiLinkageThr) {
          seeds[[i]] <- total
          seeds <- seeds[-j]
          changed <- TRUE
          break
        }
      }
      if(changed) {
        break
      }
    }
    newSeedCount <- length(seeds)
    if(debug) {
      cat(sprintf("lastSeedCount=%d, newSeedCount=%d, i=%d, j=%d\n", lastSeedCount, newSeedCount, i, j))
    }
  }
  }
  ## sort the seeds
  seeds <- lapply(seeds, sort, decreasing=FALSE)
  return(seeds)
}
