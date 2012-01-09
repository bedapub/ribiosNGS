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
