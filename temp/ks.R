## despite we set a S4 class, we do not use it internally:
## for it is very slow (~30x) compared to native R numeric vector
##setClass("ConnectivityScore",
##         representation(ksUp="numeric",
##                      ksDown="numeric"),
##         contains="numeric")
         
ksScore <- function(n, vec) {
  vec <- sort(vec, decreasing=FALSE)
  vect <- length(vec)
  vecs <- seq(along=vec)
  a <- max(sapply(vecs,
                  function(x) x/vect - vec[x]/n))
  b <- max(sapply(vecs,
                  function(x) vec[x]/n-(x-1)/vect))
  return(ifelse(a>=b, a, -b))
}

indexConnScore <- function(n, ind.up, ind.down) {
  if(all(is.na(ind.up)) || all(is.na(ind.down)))
    return(c(cscore=0, ksup=NA, ksdown=NA))
  ksup <- ksScore(n=n, vec=ind.up)
  ksdown <- ksScore(n=n, vec=ind.down)
  if(sign(ksup)==sign(ksdown)) {
    cscore <- 0
  } else {
    cscore <- ksup - ksdown
  }
  return(c(cscore=cscore, ksup=ksup, ksdown=ksdown))
}

indexConnScorePerm <- function(n, n.up, n.down, B=1000) {
  t(sapply(1:B, function(b) {
    vup <- sample(1:n, n.up, replace=TRUE)
    vdown <- sample(1:n, n.down, replace=TRUE)
    indexConnScore(n=n, ind.up=vup, ind.down=vdown)
  }))
}

## rnk: genes in the descending order with respect of their scores
## (change from data.frame before)
## TODO: this function can be further optimized. Now the perm has to be called via sapply. It should be done within the function, so as to save the overhead of calling functions.

## rnk, up, down: characters
connScore <- function(rnk, up, down) {
  n <- length(rnk)
  vup <- match(up, rnk)
  vdown <- match(down,rnk)
  indexConnScore(n=n, ind.up=vup, ind.down=vdown)
}

## method for character: less preferable than indexConnScorePerm
connScorePerm <- function(rnk, up, down, B=1000) {
  n <- length(rnk)
  n.up <- length(up)
  n.down <- length(down)
  indexConnScorePerm(n=n, n.up=n.up, n.down=n.down, B=B)
}

## rnks: list of characters
## up, down: characters
## scaling: logical, whether the connectivity score should be scaled to [-1,+1]
## permB: Number of permutations (0 for no permutation)
## sortBy: output data.frame should be sorted by which column

cmap <- function(rnks, up, down,
                 scaling=TRUE,
                 permB=0,
                 sortBy=c("default", "none", "p")) {
  sortBy <- match.arg(sortBy)
  cscoresAll <- lapply(rnks, function(x) connScore(x, up=up, down=down))
  cscores <- sapply(cscoresAll, "[[", 1)
  
  ## permutation: only applied to cases where connScore!=0
  if(permB>0) {
    n.up <- length(up)
    n.down <- length(down)
    non0.cscore <- which(cscores != 0)
    cscoreP <- numeric(length(rnks))
    cscoreP[cscores==0] <- 1
    if(length(non0.cscore)>0) {
      cscorePerm <- lapply(rnks[non0.cscore],
                           function(x) indexConnScorePerm(n=length(x), n.up=n.up, n.down=n.down, B=permB)[, 1L])
      cscoreP[non0.cscore] <- sapply(seq(along=non0.cscore),
                                     function(i) {
                                       cs <- cscores[ non0.cscore[i] ]
                                       mean(abs(cscorePerm[[i]])>=abs(cs))
                                     })
    }
    cscoreFDR <- p.adjust(cscoreP, "BH")
  } else {
    cscoreP <- cscoreFDR <- rep(as.numeric(NA), length(rnks))
  }

  ksups <- sapply(cscoresAll, "[[", 2)
  ksdowns <- sapply(cscoresAll, "[[", 3)
  rind <- seq(along=rnks)
  S <- vector("numeric", length(rnks))
  
  if(scaling) {
    p <- max(cscores)
    q <- min(cscores)
    S[cscores>0] <- cscores[cscores>0]/p
    S[cscores<0] <- -cscores[cscores<0]/q
  } else {
    S <- cscores
  }
  
  ind <- rind[order(S, ksups, decreasing=TRUE)]
  res <- data.frame(index=ind,
                    connScore=S[ind],
                    raw.connScore=cscores[ind],
                    ksup=ksups[ind],
                    ksdown=ksdowns[ind],
                    p=cscoreP[ind],
                    FDR=cscoreFDR[ind])
  if(sortBy=="none") {
    res <- res[order(res$index),,drop=FALSE]
  } else if (sortBy=="p") {
    res <- res[order(res$p, decreasing=FALSE),,drop=FALSE]
  }
  return(res)
}
