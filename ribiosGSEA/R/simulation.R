## class definition
validateTwoGroupExprsSimulator <- function(object) {
    if(length(object@nGenes)<=1) {
        warning("nGenes length less than 1")
        return(FALSE)
    }
    if(length(object@nSamples)!=2) {
        warning("nSamples must be of two values")
        return(FALSE)
    }
    if(length(object@tpGeneSetInd)<1) {
        warning("tpGeneSetInd length less than 1")
        return(FALSE)
    }
    if(length(object@tpGeneSetCor)!=2) {
        warning("tpGeneSetCor length must equal 2")
        return(FALSE)
    }
    if(length(object@bgDgeInd)!=length(object@bgDgeDeltaMean)) {
        warning("bgDgeDeltaMean length does not match bgDgeInd length")
        return(FALSE)
    }
    if(length(object@bgCorInd)!=length(object@bgCorSigma)) {
        warning("bgCorSigma length does not match bgCorInd length")
        return(FALSE)
    }
    if(length(object@bgCorInd)!=length(object@bgCorCluster)) {
        warning("bgCorCluster length does not match bgCorInd length")
        return(FALSE)
    }
    return(TRUE)
}
setClass("TwoGroupExprsSimulator",
         representation(nGenes="integer",
                        nSamples="integer",
                        tpGeneSetInd="integer",
                        deltaMean="numeric",
                        tpGeneSetCor="numeric",
                        bgDgeInd="integer", ## genes not in tpGeneSet that are differentially expressed
                        bgDgeDeltaMean="numeric",  ## deltaMean of bgDgeDelt
                        bgCorInd="integer", ## genes not in tpGeneSet that are correlated
                        bgCorCluster="factor", ## correlation cluster
                        bgCorSigma="numeric", ## sigma coefficient of the correlation cluster
                        randomSeed="integer",
                        matrix="matrix"),
         prototype=list(nGenes=12000L,
             nSamples=c(3L,3L),
             tpGeneSetInd=1:20,
             deltaMean=1,
             tpGeneSetCor=c(0,0),
             bgDgeInd=integer(),
             bgDgeDeltaMean=numeric(),
             bgCorInd=integer(),
             bgCorCluster=factor(),
             bgCorSigma=numeric(),
             randomSeed=1887L,
             matrix=matrix()),
         validity=validateTwoGroupExprsSimulator)

setClass("BenchmarkDataset",
         representation("simulators"="list",
                        "genesets"="list"))
setClass("Benchmarker",
         representation("pFunc"="function"),
         contains="BenchmarkDataset")

validBenchmarkResult <- function(object) {
    if(ncol(object@ROC)!=3) {
        warning("'ROC' must be of three columns")
        return(FALSE)
    }
    if(!identical(colnames(object@ROC),c("pThr", "FPR", "TPR"))) {
        warning("'ROC' must contain 3 columns: pThr, FPR, TPR")
        return(FALSE)
    }
    return(TRUE)
}
setClass("BenchmarkResult",
         representation(ROC="data.frame",
                        AUC="numeric",
                        ranks="numeric"),
         validity=validBenchmarkResult)

## methods
setGeneric("nGenes", function(object) standardGeneric("nGenes"))
setGeneric("nSamples", function(object) standardGeneric("nSamples"))
setGeneric("tpGeneSetInd", function(object) standardGeneric("tpGeneSetInd"))
setGeneric("deltaMean", function(object) standardGeneric("deltaMean"))
setGeneric("tpGeneSetCor", function(object) standardGeneric("tpGeneSetCor"))
setGeneric("bgDgeInd", function(object) standardGeneric("bgDgeInd"))
setGeneric("bgDgeLength", function(object) standardGeneric("bgDgeLength"))
setGeneric("bgDgePerc", function(object) standardGeneric("bgDgePerc"))
setGeneric("bgDgeDeltaMean", function(object) standardGeneric("bgDgeDeltaMean"))
setGeneric("bgCorLength", function(object) standardGeneric("bgCorLength"))
setGeneric("bgCorInd", function(object) standardGeneric("bgCorInd"))
setGeneric("bgCorPerc", function(object) standardGeneric("bgCorPerc"))
setGeneric("bgCorCluster", function(object) standardGeneric("bgCorCluster"))
setGeneric("bgCorSigma", function(object) standardGeneric("bgCorSigma"))
setGeneric("randomSeed", function(object) standardGeneric("randomSeed"))

setGeneric("nGenes<-", function(object,value) standardGeneric("nGenes<-"))
setGeneric("nSamples<-", function(object,value) standardGeneric("nSamples<-"))
setGeneric("tpGeneSetInd<-", function(object,value) standardGeneric("tpGeneSetInd<-"))
setGeneric("deltaMean<-", function(object,value) standardGeneric("deltaMean<-"))
setGeneric("tpGeneSetCor<-", function(object,value) standardGeneric("tpGeneSetCor<-"))
setGeneric("bgDgeInd<-", function(object,value) standardGeneric("bgDgeInd<-"))
setGeneric("bgDgeDeltaMean<-", function(object,value) standardGeneric("bgDgeDeltaMean<-"))
setGeneric("bgCorInd<-", function(object,value) standardGeneric("bgCorInd<-"))
setGeneric("bgCorCluster<-", function(object,value) standardGeneric("bgCorCluster<-"))
setGeneric("bgCorSigma<-", function(object,value) standardGeneric("bgCorSigma<-"))
setGeneric("randomSeed<-", function(object,value) standardGeneric("randomSeed<-"))

setMethod("nGenes", "TwoGroupExprsSimulator", function(object) object@nGenes)
setMethod("nSamples", "TwoGroupExprsSimulator", function(object) object@nSamples)
setMethod("tpGeneSetInd", "TwoGroupExprsSimulator", function(object) object@tpGeneSetInd)
setMethod("deltaMean", "TwoGroupExprsSimulator", function(object) object@deltaMean)
setMethod("bgDgeInd", "TwoGroupExprsSimulator", function(object) object@bgDgeInd)
setMethod("bgDgeLength", "TwoGroupExprsSimulator", function(object) length(object@bgDgeInd))
setMethod("bgDgePerc", "TwoGroupExprsSimulator", function(object) length(object@bgDgeInd)/object@nGenes)
setMethod("bgCorInd", "TwoGroupExprsSimulator", function(object) object@bgCorInd)
setMethod("bgCorLength", "TwoGroupExprsSimulator", function(object) length(object@bgCorInd))
setMethod("bgCorPerc", "TwoGroupExprsSimulator", function(object) length(object@bgCorInd)/object@nGenes)
setMethod("bgCorCluster", "TwoGroupExprsSimulator", function(object) object@bgCorCluster)
setMethod("bgCorSigma", "TwoGroupExprsSimulator", function(object) object@bgCorSigma)
setMethod("bgDgeDeltaMean", "TwoGroupExprsSimulator", function(object) { object@bgDgeDeltaMean })
setMethod("tpGeneSetCor", "TwoGroupExprsSimulator", function(object) object@tpGeneSetCor)
setMethod("randomSeed", "TwoGroupExprsSimulator", function(object) object@randomSeed)

setMethod("ncol", "TwoGroupExprsSimulator", function(x) ncol(x@matrix))
setMethod("nrow", "TwoGroupExprsSimulator", function(x) nrow(x@matrix))

setMethod("nGenes<-",
          c("TwoGroupExprsSimulator", "numeric"), function(object,value) {
              object@nGenes<-as.integer(value)
              return(object)
          })
setMethod("nSamples<-",
          c("TwoGroupExprsSimulator", "numeric"), function(object, value) {
              if(length(value)>2)
                  stop("value must be of length 2")
              if(length(value)==1) {
                  value <- c(value,value)
              }
              value <- as.integer(value)
              object@nSamples <- value
              return(object)
          })
setMethod("tpGeneSetInd<-",
          c("TwoGroupExprsSimulator", "numeric"), function(object,value) {
              object@tpGeneSetInd <- as.integer(value)
              return(object)
          })
setMethod("deltaMean<-",
          c("TwoGroupExprsSimulator", "numeric"), function(object,value) {
              object@deltaMean <- value
              return(object)
          })
setMethod("tpGeneSetCor<-",
          c("TwoGroupExprsSimulator", "numeric"), function(object,value) {
              if(length(value)>2)
                  stop("value must be of length 2")
              if(length(value)==1) {
                  value <- c(value,value)
              }
              object@tpGeneSetCor <- value
              return(object)
          })
setMethod("randomSeed<-",
          c("TwoGroupExprsSimulator", "numeric"), function(object,value) {
              object@randomSeed <- as.integer(value)
              return(object)
          })
setMethod("bgDgeInd<-", c("TwoGroupExprsSimulator", "numeric"), function(object, value) {
              object@bgDgeInd <- as.integer(value)
              return(object)
          })
setMethod("bgDgeDeltaMean<-", c("TwoGroupExprsSimulator", "numeric"), function(object, value) {
              if(length(object@bgDgeInd)==0 && length(value)!=0)
                  stop("bgDgeDeltaMean must be specified after bgDgeInd")
              object@bgDgeDeltaMean <- rep_len(value, length(object@bgDgeInd))
              return(object)
          })
setMethod("bgCorInd<-", c("TwoGroupExprsSimulator", "numeric"), function(object, value) {
              object@bgCorInd <- as.integer(value)
              return(object)
          })
setMethod("bgCorCluster<-", c("TwoGroupExprsSimulator", "factor"), function(object, value) {
              object@bgCorCluster <- value
              return(object)
          })
setMethod("bgCorSigma<-", c("TwoGroupExprsSimulator", "numeric"), function(object, value) {
              if(length(object@bgCorInd)==0 && length(value)!=0)
                  stop("bgCorSigma must be specified after bgCorInd")
              object@bgCorSigma <- rep_len(value, length(object@bgCorInd))
              return(object)
          })
setMethod("designMatrix", "TwoGroupExprsSimulator", function(object) {
              matrix(c(rep(1, sum(nSamples(object))),
                       rep(c(0,1), nSamples(object))),
                     ncol=2, byrow=FALSE)
          })
setMethod("contrastMatrix", "TwoGroupExprsSimulator", function(object) {
              matrix(c(0,1), byrow=FALSE, ncol=1)
          })

setMethod("show", "TwoGroupExprsSimulator", function(object) {
              cat("TwoGroupExprsSimulator with", nGenes(object), "genes and", paste(nSamples(object),collapse="+"), "samples\n")
              cat("[+] ", length(tpGeneSetInd(object)), " true positive genes with deltaMean=", deltaMean(object),
                  ". Use 'tpGeneSetInd(object)' to see them.\n", sep="")
              cat("[+]", bgDgeLength(object), "differentially expressed background genes. Use 'bgDgeInd(object)' to see them.\n")
              cat("[+]", bgCorLength(object), "correlated background genes in", nlevels(bgCorCluster(object)), "clusters. Use 'bgCorInd(object)' to see them.\n")
              cat("[+] Random seed:", randomSeed(object), "\n")
              cat("[+] Matrix: use 'exprs(object)' see the expression matrix\n",sep="")
          })

setGeneric("simulators", function(object) standardGeneric("simulators"))
setGeneric("genesets", function(object) standardGeneric("genesets"))
setMethod("simulators", "BenchmarkDataset", function(object) object@simulators)
setMethod("genesets", "BenchmarkDataset", function(object) object@genesets)

setGeneric("pFunc", function(object) standardGeneric("pFunc"))
setGeneric("pFunc<-", function(object,value) standardGeneric("pFunc<-"))
setGeneric("pValues", function(object) standardGeneric("pValues"))
setMethod("pFunc", "Benchmarker", function(object) return(object@pFunc));
setMethod("pFunc<-", c("Benchmarker", "function"), function(object,value) {
              object@pFunc <- value
              return(object)
          })

setMethod("pValues", "Benchmarker", function(object) {
              lapply(simulators(object), function(sim) {
                         do.call(pFunc(object),
                                 list(sim, index=genesets(object)))
                     })
          })

setGeneric("benchmark", function(object) standardGeneric("benchmark"))
setGeneric("ROC", function(object) standardGeneric("ROC"))
setGeneric("AUC", function(object) standardGeneric("AUC"))
setGeneric("ranks", function(object) standardGeneric("ranks"))
setGeneric("avgRank", function(object) standardGeneric("avgRank"))
setGeneric("minRank", function(object) standardGeneric("minRank"))
setGeneric("maxRank", function(object) standardGeneric("maxRank"))
setGeneric("rankStat", function(object) standardGeneric("rankStat"))

setMethod("ROC", "BenchmarkResult", function(object) object@ROC)
setMethod("AUC", "BenchmarkResult", function(object) object@AUC)
setMethod("ranks", "BenchmarkResult", function(object) object@ranks)
setMethod("avgRank", "BenchmarkResult", function(object) mean(object@ranks, na.rm=TRUE))
setMethod("minRank", "BenchmarkResult", function(object) min(object@ranks))
setMethod("maxRank", "BenchmarkResult", function(object) max(object@ranks))
setMethod("rankStat", "BenchmarkResult", function(object) paste("min=",minRank(object),
                                                                "/avg=", avgRank(object),
                                                                "/max=", maxRank(object), sep=""))

newBenchmarkResult <- function(ROC, AUC, ranks) {
    return(new("BenchmarkResult",
               ROC=ROC, AUC=AUC, ranks=ranks))
}

setMethod("benchmark", "Benchmarker", function(object) {
              pval <- pValues(object)
              tpVals <- sapply(pval, "[[", 1L)
        
              pStep <- 0.001
              ps <- seq(0, 1, pStep)
              fpr <- sapply(ps, function(p0) mean(sapply(pval, function(pp) mean(pp[-1]<p0))))
              tpr <- sapply(ps, function(p0) mean(tpVals<=p0))
              roc <- data.frame(pThr=ps,
                                FPR=fpr,
                                TPR=tpr)
              auc <- sum(tpr)*pStep
              ranks <- sapply(pval, function(x) rank(x)[1])
              res <- new("BenchmarkResult", ROC=roc, AUC=auc, ranks=ranks)
              return(res)
          })

## functions
as.matrix.TwoGroupExprsSimulator <- function(x) return(x@matrix)
setAs(from="TwoGroupExprsSimulator", to="matrix", function(from) return(from@matrix))
setMethod("exprs", "TwoGroupExprsSimulator", function(object) { return(object@matrix)})
setMethod("exprs<-", "TwoGroupExprsSimulator", function(object,value) { object@matrix<-value; return(object)})

isGroup2 <- function(tgSim) 1:ncol(tgSim)>nSamples(tgSim)[1]

sigmaMatrix <- function(cor, n) {
    res <- matrix(cor, nrow=n, ncol=n)
    diag(res) <- 1L
    return(res)
}

mvrnormMatrix <- function(nr, nc, mu, sigma) {
    mu <- rep_len(mu, length.out=nr)
    sigmaMat <- sigmaMatrix(sigma, nr)
    resMat <- t(mvrnorm(n=nc,
                        mu=mu,
                        Sigma=sigmaMat))
    return(resMat)
}


twoGroupMvrnorm <- function(nGenes, nSamples, deltaMean, sigma) {
    stopifnot(length(nSamples)==2)
    deltaMean <- rep_len(deltaMean, length(nGenes))
    if(length(sigma)==1)
        sigma <- rep(sigma, 2L)
    if(length(sigma)>=3)
        stop("sigma must be of length 1 or 2")
    
    sigmaMat1 <- mvrnormMatrix(nGenes, nSamples[1], 0, sigma[1])
    sigmaMat2 <- mvrnormMatrix(nGenes, nSamples[2], deltaMean, sigma[2])
    res <- cbind(sigmaMat1, sigmaMat2)
    return(res)
}

rnormMatrix <- function(nr, nc, mu=0, sd=1) {
     matrix(rnorm(nr*nc, mean=mu, sd=sd), nrow=nr, byrow=FALSE)
}

twoGroupRnorm <- function(nGenes, nSamples, deltaMean, sd=1) {
    stopifnot(length(nSamples)==2)
    deltaMean <- rep_len(deltaMean, length(nGenes))
    if(length(sd)==1)
        sd <- rep(sd, 2L)
    if(length(sd)>=3)
        stop("sd must be of length 1 or 2")
    rmat1 <- rnormMatrix(nGenes, nSamples[1], 0, sd)
    rmat2 <- rnormMatrix(nGenes, nSamples[2], deltaMean, sd)
    res <- cbind(rmat1, rmat2)
    return(res)
}


avgCor <- function(mat) {corMat <- cor(t(mat)); mean(corMat[upper.tri(corMat)])}
mutateBg <- function(tgSim) {

    set.seed(randomSeed(tgSim))
    
    bgDgeInd <- bgDgeInd(tgSim)
    bgDgeDeltaMean <- bgDgeDeltaMean(tgSim)
    bgCorInd <- bgCorInd(tgSim)
    bgCorCluster <- bgCorCluster(tgSim)
    bgCorSigma <- bgCorSigma(tgSim)

    tpGeneInd <- tpGeneSetInd(tgSim)

    nG <- nGenes(tgSim)
        
    intDgeCor <- intersect(bgDgeInd, bgCorInd)
    if(length(intDgeCor)>0) {
        bgDgeIsInt <- bgDgeInd %in% intDgeCor
        bgCorIsInt <- bgCorInd %in% intDgeCor

        intDeltaMean <- bgDgeDeltaMean[match(intDgeCor, bgDgeInd)]
        intCorCluster <- droplevels(bgCorCluster[match(intDgeCor, bgCorInd)])
        intCorSigma <- bgCorSigma[match(intDgeCor, bgCorInd)]
        intDeltaMeanByCluster <- split(intDeltaMean, intCorCluster)
        intIndByCorCluster <- split(intDgeCor, intCorCluster)
        intSigmaByCorCluster <- split(intCorSigma, intCorCluster)

        for(i in seq(along=intIndByCorCluster)) {
            inds <- intIndByCorCluster[[i]]
            sigma <- intSigmaByCorCluster[[i]][1]
            deltaMean <- intDeltaMeanByCluster[[i]]
            set.seed(randomSeed(tgSim))
            intCorRep <- twoGroupMvrnorm(nGenes=length(inds),
                                         nSamples=nSamples(tgSim),
                                         deltaMean=deltaMean,
                                         sigma=sigma)
            tgSim@matrix[inds,] <- intCorRep
        }
        bgDgeInd <- bgDgeInd[!bgDgeIsInt]
        bgDgeDeltaMean <- bgDgeDeltaMean[!bgDgeIsInt]

        bgCorInd <- bgCorInd[!bgCorIsInt]
        bgCorCluster <- droplevels(bgCorCluster[!bgCorIsInt])
        bgCorSigma <- bgCorSigma[!bgCorIsInt]
    }
    if(length(bgDgeInd)>0) {
        stopifnot(length(bgDgeInd)==length(bgDgeDeltaMean))
        tgSim@matrix[bgDgeInd,] <-  twoGroupRnorm(length(bgDgeInd),
                                                  nSamples(tgSim),
                                                  bgDgeDeltaMean)

    }
    if(length(bgCorInd)>0) {
        indByCluster <- split(bgCorInd,bgCorCluster)
        sigmaByCluster <- split(bgCorSigma,bgCorCluster)
        for(i in seq(along=indByCluster)) {
            inds <- indByCluster[[i]]
            sigma <- sigmaByCluster[[i]][1]
            set.seed(randomSeed(tgSim))
            corRep <- twoGroupMvrnorm(nGenes=length(inds),
                                      nSamples=nSamples(tgSim),
                                      deltaMean=0L,
                                      sigma=sigma)
            tgSim@matrix[inds,] <- corRep
        }
    }

    return(tgSim)
}

mutateBgByParams <- function(tgSim,
                             bgDgeInd,
                             bgDgeDeltaMean ,
                             bgCorInd,
                             bgCorCluster,
                             bgCorSigma) {

    if(!missing(bgDgeInd)) bgDgeInd(tgSim) <- bgDgeInd
    if(!missing(bgDgeDeltaMean)) bgDgeDeltaMean(tgSim) <- bgDgeDeltaMean
    if(!missing(bgCorInd)) bgCorInd(tgSim) <- bgCorInd
    if(!missing(bgCorCluster)) bgCorCluster(tgSim) <- bgCorCluster
    if(!missing(bgCorSigma)) bgCorSigma(tgSim) <- bgCorSigma

    mutateBg(tgSim)
}


## test <- twoGroupMvrnormMatrix(5,c(3,3), 1,0.8)

simulateTwoGroupData <- function(tgSim) {
    set.seed(randomSeed(tgSim))
    Ngenes <- nGenes(tgSim)
    Nsamples <- nSamples(tgSim)
    mat <- matrix(rnorm(Ngenes*sum(Nsamples)), nrow=Ngenes)
    delta <- deltaMean(tgSim)

    tpGeneInd <- tpGeneSetInd(tgSim)
    tpGeneCount <- length(tpGeneInd)
    tpCor <- tpGeneSetCor(tgSim)

    if(length(delta)!=tpGeneCount)
        delta <- rep_len(delta, tpGeneCount)

    isG2 <- isGroup2(tgSim)
    set.seed(randomSeed(tgSim))
    if(all(tpCor==0)) {
        mat[tpGeneInd,] <- twoGroupRnorm(length(tpGeneInd),
                                             nSamples(tgSim),
                                             deltaMean=delta)
    } else {

        mat[tpGeneInd,] <- twoGroupMvrnorm(nGenes=tpGeneCount,
                                           nSamples=Nsamples,
                                           deltaMean=delta,
                                           sigma=tpCor)
    }

    ## mutate
    rownames(mat) <- paste("Gene", 1:Ngenes, sep="")
    colnames(mat) <- paste("Group", rep(c(1,2), Nsamples), ".Sample", c(1:Nsamples[1], 1:Nsamples[2]),sep="")

    exprs(tgSim) <- mat
    tgSim <- mutateBg(tgSim)
    return(tgSim)
}


randomGroup <- function(n, ranges=3:30) {
    ind <- 1:n
    nGroup <- ceiling(n/min(ranges))
    rs <- sample(ranges, nGroup, replace=TRUE)
    left <- min(which(cumsum(rs)>=n))
    res <- rep(1:left, rs[1:left])[ind]
    return(factor(res))
}

defaultBgDgeDeltaMeanFunc <- function(n) rnorm(n, mean=0, sd=1)
defaultByCorSigmaFunc <- function(n) runif(n, min=0, max=1)

randomlyMutateBg <- function(tgSim,
                             bgDgePerc=0,
                             bgCorPerc=0,
                             bgDgeCorPerc=0,
                             bgDgeDeltaMeanFunc=defaultBgDgeDeltaMeanFunc,
                             bgCorSigmaFunc=defaultByCorSigmaFunc) {
    stopifnot(bgDgeCorPerc<=bgDgePerc && bgDgeCorPerc<=bgCorPerc)
    
    nG <- nGenes(tgSim)
    allInds <- setdiff(1:nGenes(tgSim), tpGeneSetInd(tgSim))
    set.seed(randomSeed(tgSim))

    nBgDge <- ceiling(nG * bgDgePerc)
    nBgCor <- ceiling(nG * bgCorPerc)
    nBgCorDge <- ceiling(nG * bgDgeCorPerc)

    nSel <- ceiling(nG * (bgDgePerc+bgCorPerc-bgDgeCorPerc))
    if(nSel>=length(allInds)) {
        stop("Not enough genes in the background. Set bgDgePerc/bgCorPerc to a lower level")
    }
    selInds <- sample(allInds, nSel, replace=FALSE)

    if(bgDgePerc>0) {
        bgDgeInd(tgSim) <- selInds[1:nBgDge]
        bgDgeDeltaMean(tgSim) <- do.call(bgDgeDeltaMeanFunc, list(nBgDge))
    }

    if(bgCorPerc>0) {
        corInds <- selInds[(nBgDge+1-nBgCorDge):length(selInds)]
        bgCorInd(tgSim) <- corInds
        corCluster <- randomGroup(length(corInds))
        bgCorCluster(tgSim) <- corCluster
        bgCorSigmaBase <- do.call(bgCorSigmaFunc, list(nlevels(corCluster)))
        bgCorSigma(tgSim) <- bgCorSigmaBase[as.integer(corCluster)]
    }
    
    ## mutate background
    res <- mutateBg(tgSim)
    return(res)
}

newTwoGroupExprsSimulator <- function(nGenes=12000,
                                      nSamples=c(3,3),
                                      tpGeneSetInd=1:20,
                                      deltaMean=1.0,
                                      tpGeneSetCor=0,
                                      bgDgeInd=integer(),
                                      bgDgeDeltaMean=numeric(),
                                      bgCorInd=integer(),
                                      bgCorCluster=factor(),
                                      bgCorSigma=numeric(),
                                      randomSeed=1887) {

    ## parameter check
    if(any(bgDgeInd %in% tpGeneSetInd) || any(bgCorInd %in% tpGeneSetInd))
        stop("Background gene cannot overlap with true-positive gene set genes")
    stopifnot(length(bgCorInd)==length(bgCorCluster))

    obj <- new("TwoGroupExprsSimulator")
    nGenes(obj) <- nGenes
    nSamples(obj) <- nSamples
    tpGeneSetInd(obj) <- tpGeneSetInd
    deltaMean(obj) <- deltaMean
    tpGeneSetCor(obj) <- tpGeneSetCor
    bgDgeInd(obj) <- bgDgeInd
    bgDgeDeltaMean(obj) <- bgDgeDeltaMean
    bgCorInd(obj) <- bgCorInd
    bgCorCluster(obj) <- bgCorCluster
    bgCorSigma(obj) <- bgCorSigma
    randomSeed(obj) <- randomSeed

    obj <- simulateTwoGroupData(obj)
    return(obj)
}

setGeneric("cloneTwoGroupExprsSimulator", function(object, randomSeed,...) standardGeneric("cloneTwoGroupExprsSimulator"))
setMethod("cloneTwoGroupExprsSimulator", c("TwoGroupExprsSimulator", "numeric"), function(object, randomSeed) {
              randomSeed(object) <- randomSeed
              object <- simulateTwoGroupData(object)
              return(object)
          })
setMethod("cloneTwoGroupExprsSimulator", c("TwoGroupExprsSimulator", "missing"), function(object, randomSeed) {
              return(object)
          })


## pFuncs: pFunc is a function that accepts an TwoGroupExprsSimulator object and a list of gene set indices
## expected return: p-values of the gene sets in the same order as a numeric vector

getFisherP <- function(geneInd, regInd, nGenes) {
    x00 <- intersect(geneInd, regInd)
    x01 <- setdiff(regInd, geneInd)
    x10 <- setdiff(geneInd, regInd)
    x11 <- nGenes - length(union(geneInd, regInd))
    x <- matrix(c(length(x00), length(x01),
                  length(x10),x11), nrow=2, byrow=TRUE)
    fisher.test(x, alternative="greater")$p.value
}
pFuncFisherExact <- function(tgSim, index) {
    fit <- lmFit(as.matrix(tgSim), design=designMatrix(tgSim))
    fit <- contrasts.fit(fit, contrasts=contrastMatrix(tgSim))
    fit <- eBayes(fit)
    tt <- topTable(fit, number=length(fit$p.value))
    genes <- rownames(fit$p.value)
    tt <- tt[genes,]
    dge <- limmaTopTable2dgeTable(tt)
    trunc <- truncateDgeTable(dge)
    pos <- match(trunc$pos$Feature, genes)
    neg <- match(trunc$neg$Feature, genes)
    posEnrich <- sapply(index, function(x) getFisherP(x, pos, length(genes)))
    ## negEnrich <- sapply(index, function(x) getFisherP(x, neg, length(genes)))
    ## pEnrich <- pmin(posEnrich, negEnrich)
    pEnrich <- posEnrich
    return(pEnrich)
}

pFuncCamera <- function(tgSim, index) {
    res <- camera(as.matrix(tgSim),
                  index=index,
                  design=designMatrix(tgSim),
                  contrast=contrastMatrix(tgSim),
                  sort=FALSE)
    return(res$PValue)
}

pFuncCameraRank <- function(tgSim, index) {
    res <- camera(as.matrix(tgSim),
                  index=index,
                  design=designMatrix(tgSim),
                  contrast=contrastMatrix(tgSim),
                  sort=FALSE, use.ranks=TRUE)
    return(res$PValue)
}


pFuncBioQCtStat <- function(tgSim, index) {
    wmwRes <- wmwTest(as.matrix(tgSim), index, alternative="Q")
    fit <- lmFit(wmwRes, design=designMatrix(tgSim))
    fit <- contrasts.fit(fit, contrastMatrix(tgSim))
    fit <- eBayes(fit)
    res <- fit$p.value[,1]
    return(res)
}


pFuncMroast <- function(tgSim, index) {
    res <- mroast.default(as.matrix(tgSim),
                  index,
                  design=designMatrix(tgSim),
                  contrast=contrastMatrix(tgSim), sort="none")
    return(res$PValue)
}

pFuncRomer <- function(tgSim, index) {
    res <- romer(as.matrix(tgSim),
                 index=index,
                 design=designMatrix(tgSim),
                 contrast=contrastMatrix(tgSim), nrot=199)
    return(res[,"Mixed"])
}

pFuncGlobaltest <- function(tgSim, index) {
    Y <- factor(designMatrix(tgSim)[,2L])
    X <- t(as.matrix(tgSim))
    gtObj <- gt(Y, X, subsets=index)
    p <- gtObj@result[,"p-value"]
    return(p)
}

ztest <- function(stats) {
  ms <- mean(stats, na.rm=TRUE)
  eg <- sqrt(length(stats)) * ms
  p.less <- pnorm(eg, lower.tail=TRUE)
  p.greater <- 1-p.less
  p.twosided <- pmin(p.less, p.greater)*2
  res <- c(statistic=eg,
           count=length(stats),
           p.less=p.less,
           p.greater=p.greater,
           p.twosided=p.twosided)
  return(res)
}

tgSim2limmaFit <- function(tgSim) {
    fit <- lmFit(as.matrix(tgSim), design=designMatrix(tgSim))
    fit <- contrasts.fit(fit, contrasts=contrastMatrix(tgSim))
    fit <- eBayes(fit)
    return(fit)
}

fisherMethod <- function(pValues) {
    df <- 2*length(pValues)
    pchisq( -2*sum(log(pValues)), df, lower.tail=FALSE)
}

pFuncFisherMethod <- function(tgSim, index) {
    fit <- tgSim2limmaFit(tgSim)
    pGenes <- fit$p.value
    fisherP <- sapply(index, function(x) fisherMethod(pGenes[x]))
    return(fisherP)
}

pFuncTstatZtest <- function(tgSim, index) {
    fit <- tgSim2limmaFit(tgSim)
    tVals <- fit$t[,1]
    pZtest <- sapply(index, function(x) ztest(tVals[x])["p.twosided"])
    return(pZtest)
}

pFuncTstatTtest <- function(tgSim, index) {
    fit <- tgSim2limmaFit(tgSim)
    tVals <- fit$t[,1]
    pTtest <- sapply(index, function(x) t.test(tVals[x], tVals[-x], alternative="two.sided")$p.value)
    return(pTtest)
}

pFuncTstatWMW <- function(tgSim, index) {
    fit <- tgSim2limmaFit(tgSim)
    tVals <- fit$t[,1]
    p <- unname(wmwTest(tVals, index, alternative="two.sided"))
    return(p)
}

## Chiseq
chisqTest <- function(stats) {
  ms <- mean(stats, na.rm=TRUE)
  count <- length(stats)
  statistic <- (sum((stats - ms)^2)-(count-1))/(2*(count-1))

  p.less <- pnorm(statistic, lower.tail=TRUE)
  p.greater <- 1-p.less
  p.twosided <- pmin(p.less, p.greater)*2
  res <- c(statistic=statistic,
           count=length(stats),
           p.less=p.less,
           p.greater=p.greater,
           p.twosided=p.twosided)
  return(res)
}

pFuncTstatChisq <- function(tgSim, index) {
    fit <- tgSim2limmaFit(tgSim)
    tVals <- fit$t[,1]
    pChisq <- sapply(index, function(x) chisqTest(tVals[x])["p.twosided"])
    return(pChisq)
}

## a aggregated method to save time
pFuncLimmaAggregated <- function(tgSim, index) {
    fit <- tgSim2limmaFit(tgSim)
    pGenes <- fit$p.value
    pFisherMethod <- sapply(index, function(x) fisherMethod(pGenes[x]))
    tVals <- fit$t[,1]
    pZtest <- unname(sapply(index, function(x) ztest(tVals[x])["p.twosided"]))
    pTtest <- unname(sapply(index, function(x) t.test(tVals[x], tVals[-x], alternative="two.sided")$p.value))
    pWmw <- unname(wmwTest(tVals, index, alternative="two.sided"))
    pChisq <- unname(sapply(index, function(x) chisqTest(tVals[x])["p.twosided"]))
    res <- list(pFisherMethod=pFisherMethod,
                pZtest=pZtest,
                pTtest=pTtest,
                pWmw=pWmw,
                pChisq=pChisq)
    return(res)
}





newBenchmarkDataset <- function(simulators, genesets) {
    new("BenchmarkDataset", simulators=simulators, genesets=genesets)
}

generateBenchmarkGenesets <- function(tgSim, ngsr=99) {
    geneSetInd <- tpGeneSetInd(tgSim)
    set.seed(randomSeed(tgSim))
    geneSets <- c(list(tpGeneSet=geneSetInd),
                  lapply(1:ngsr, function(x) sample(setdiff(1:nGenes(tgSim), geneSetInd), length(geneSetInd))))
    names(geneSets) <- c("truePositive", paste("GeneSet", 2:length(geneSets)))
    return(geneSets)
}

generateBenchmarkData <- function(tgSim, ngsr=99, B=100, geneSets=NULL) {
    if(is.null(geneSets)) {
        geneSets <- generateBenchmarkGenesets(tgSim, ngsr=ngsr)
    }
    mySims <- lapply(1:B, function(x)
        cloneTwoGroupExprsSimulator(tgSim, randomSeed=x))
    res <- newBenchmarkDataset(mySims, geneSets)
    return(res)
}

newBenchmarker <- function(tgSim, ngsr=99, B=100, pFunc, geneSets=NULL) {
    bd <- generateBenchmarkData(tgSim, ngsr=ngsr, B=B, geneSets=geneSets)
    ber <- as(bd, "Benchmarker")
    pFunc(ber) <- pFunc
    return(ber)
}

drawAt <- function(at, pThr, x, y, ...) {
    isAt <- which.min(abs(pThr-at))
    panel.points(x[isAt], y[isAt], ...)
}
panel.benchmarkResult <- function(x,y,pThr,...) {
    panel.xyplot(x,y,...)
    panel.abline(v=c(0.01, 0.05), lty=2, col="darkgray")
    drawAt(0.05, pThr, x, y, col="black", pch=24, fill="orange", cex=1.3)
    drawAt(0.01, pThr, x, y, col="black", pch=24, fill="red", cex=1.3)

}
xyplot.BenchmarkResult <- function(x, data,...) {
    roc <- ROC(x)
    xyplot(TPR~FPR, type="l", data=roc, abline=c(0,1),
           xlab="False positive rate", ylab="True positiev rate",
           panel=panel.benchmarkResult, pThr=roc$pThr,
           key=list(space="top", columns=2,
               text=list(labels=c("nominal p=0.05", "nominal p=0.01")),
               points=list(fill=c("orange", "red"), pch=24, cex=1.5, col="black")),
           scales=list(tck=c(1,0), alternating=1L),
           ...)
}


varParData <- function(varPar=c("nGenes", "nSamples", "tpGeneSetInd", "deltaMean", "tpGeneSetCor"),
                       varParList, ...) {
    varPar <- match.arg(varPar)
    benchSimParas <- lapply(varParList, function(x) {
                                 basic <- list()
                                 basic[[varPar]] <- x
                                 return(c(basic, list(...)))
                             })
    benchData <- lapply(benchSimParas, function(x) {
                            do.call(newTwoGroupExprsSimulator, x)
                    })
    return(benchData)
}


varParPerformance <- function(varPar=c("nGenes", "nSamples", "tpGeneSetInd", "deltaMean", "tpGeneSetCor"),
                              varParList, 
                              ngsr=99, B=100, pFunc,geneSets=NULL,
                              ...) {
    varData <- varParData(varPar=varPar,
                          varParList=varParList, ...)
    performance <- lapply(varData, function(x) {
                              benchmarker <- newBenchmarker(x, ngsr=ngsr, B=B, pFunc=pFunc, geneSets=geneSets)
                              benchmarkResult <- benchmark(benchmarker)
                              return(benchmarkResult)
                          })
    return(performance)
}

colRamp <- function(cols, n) colorRampPalette(cols)(n)

plotROC <- function(performanceList,
                    cols=c("black", "red"),
                    main, key.title, values) {
    rocPlots <- lapply(performanceList, xyplot)
    rocCols <- colRamp(cols, length(performanceList))
    rocComb <- update(rocPlots[[1]], col=rocCols[1])
    if(length(rocPlots)>1) {
        for(i in 2:length(rocPlots)) {
            rocComb <- rocComb+update(rocPlots[[i]], col=rocCols[[i]])
        }
    }
    plot(update(rocComb, main=main,
            key=list(space="right", cex=0.85,title=key.title,
                text=list(labels=sprintf("%1.1f", values))
              , lines=list(col=rocCols))))
}

panel.AUC <- function(x,y,col,...) {
    panel.xyplot(x,y,pch=21, fill=col,cex=1.25, col="black",...)
    for(i in 1:(length(x)-1)) {
        panel.segments(x[i], y[i],
                      x[i+1], y[i+1], col=col[i])
    }
}
plotAUC <- function(performanceList,
                    cols=c("black", "red"),
                    main, key.title, values,
                    scales=list(tck=c(1,0), alternating=1L, x=list(at=values)),
                    ...) {
    rocCols <- colRamp(cols, length(performanceList))
    auc <- sapply(performanceList, AUC)
    xyplot(auc ~ values, col=rocCols, panel=panel.AUC,
           scales=scales,
           xlab=key.title, ylab="AUC", main=main,...)
}

panel.ranks <- function(x,y,col,...) {
    panel.dotplot(x,y,col=col,...)
    yMean <- tapply(y, x, mean)
    xUniq <- 1:ribiosUtils::ulen(x)
    ucol <- unique(col)
    barWidth <- 0.2
    for(i in 1:(length(yMean)-1)) {
        panel.segments(xUniq[i], yMean[i],
                       xUniq[i+1], yMean[i+1], col=ucol[i])
    }
    for(i in 1:length(yMean)) {
        panel.segments(xUniq[i]-barWidth, yMean[i],
                      xUniq[i]+barWidth, yMean[i], lwd=2,
                       col=ucol[i])
    }
}
plotRanks <- function(performanceList,
                    cols=c("black", "red"),
                    main, key.title, values) {
    rocCols <- colRamp(cols, length(performanceList))
    ranks <- lapply(performanceList, ranks)
    df <- data.frame(Rank=unlist(ranks),
                     Value=rep(values, sapply(ranks, length)))
    pointCols <- rep(rocCols, sapply(ranks, length))
    dotplot(Rank ~ Value, col=pointCols, data=df, horizontal=FALSE,
            panel=panel.ranks,
            scales=list(tck=c(1,0), alternating=1L, x=list(at=seq(along=values), labels=sapply(values, "[[", 1L))),
            xlab=key.title, ylab="Ranks", main=main)
}

tpDiff <- function(tgSim) {
    mat <- as.matrix(tgSim)
    ind <- tpGeneSetInd(tgSim)
    isGroup2 <- 1:sum(nSamples(tgSim))>nSamples(tgSim)[1]
    rowMeans(mat[ind,isGroup2]-mat[ind,!isGroup2])
}

