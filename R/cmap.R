cmap <- function(rnks, up, down,
                 permB=0,
                 sortBy=c("default", "none", "p")) {
  sortBy <- match.arg(sortBy)
  stopifnot(is.matrix(rnks))
  
  cscoreAll <- connScore(rnks, up, down)
  
  cscores <- cscoreAll[, 1L]
  ksups <- cscoreAll[, 2L]
  ksdowns <- cscoreAll[, 3L]
  
  ## permutation: only applied to cases where connScore!=0
  if(permB>0) {
    n.up <- length(up)
    n.down <- length(down)
    non0.cscore <- which(cscores != 0)
    cscoreP <- numeric(length(cscores))
    cscoreP[cscores==0] <- 1
    if(length(non0.cscore)>0) {
      cscorePerm <- connScorePerm(x=length(cscores), up=n.up, down=n.down, B=permB)[,1L]
      cscoreP[non0.cscore] <- sapply(seq(along=non0.cscore),
                                     function(i) {
                                       cs <- cscores[ non0.cscore[i] ]
                                       mean(abs(cscorePerm)>=abs(cs))
                                     })
    }
    cscoreFDR <- p.adjust(cscoreP, "BH")
  } else {
    cscoreP <- cscoreFDR <- rep(as.numeric(NA), length(rnks))
  }

  rind <- seq(along=cscores)
  S <- vector("numeric", length(cscores))
  
  ## scaling
  p <- max(cscores)
  q <- min(cscores)
  S[cscores>0] <- cscores[cscores>0]/p
  S[cscores<0] <- -cscores[cscores<0]/q
  
  ind <- rind[order(S, ksups, decreasing=FALSE)]
  res <- data.frame(index=ind,
                    connScore=S[ind],
                    raw.connScore=cscores[ind],
                    ksup=ksups[ind],
                    ksdown=ksdowns[ind],
                    p=cscoreP[ind],
                    FDR=cscoreFDR[ind])

  ## reordering
  if(sortBy=="none") {
    res <- res[order(res$index),,drop=FALSE]
  } else if (sortBy=="p") {
    res <- res[order(res$p, decreasing=FALSE),,drop=FALSE]
  }
  return(res)
}
