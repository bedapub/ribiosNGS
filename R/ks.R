ksScore <- function(vec, n) {
  vec <- sort(vec, decreasing=FALSE)
  vect <- length(vec)
  vecs <- seq(along=vec)
  a <- max(sapply(vecs,
                  function(x) x/vect - vec[x]/n))
  b <- max(sapply(vecs,
                  function(x) vec[x]/n-(x-1)/vect))
  if(a>=b)
    return(a)
  else
    return(-b)
}

## rnk: genes in the descending order with respect of their scores
## (change from before: it was data.frame)
## TODO: this function can be further optimized. Now the perm has to be called via sapply. It should be done within the function, so as to save the overhead of calling functions.
connScore <- function(rnk, up, down, perm=FALSE) {
  rnk <- as.character(rnk)
  up <- as.character(up)
  down <- as.character(down)
  n <- length(rnk)
  
  if(perm) {
    rnkseq <- 1:n
    vup <- sample(rnkseq, length(up), replace=TRUE)
    vdown <- sample(rnkseq, length(down), replace=TRUE)
  } else {
    stopifnot(any(up %in% rnk) & any(down %in% rnk))
    vup <- match(up, rnk)
    vdown <- match(down, rnk)
  }
  
  ksup <- ksScore(vec=vup, n=n)
  ksdown <- ksScore(vec=vdown, n=n)

  if(sign(ksup)==sign(ksdown)) {
    cscore <- 0
  } else {
    cscore <- ksup - ksdown
  }

  return(c(cscore=cscore, ksup=ksup, ksdown=ksdown))
}

cmap <- function(rnks, up, down, perm=FALSE) {
  cscoresAll <- lapply(rnks, function(x) connScore(x, up=up, down=down, perm=perm))
  cscores <- sapply(cscoresAll, "[[", 1)
  ksups <- sapply(cscoresAll, "[[", 2)
  ksdowns <- sapply(cscoresAll, "[[", 3)
  p <- max(cscores)
  q <- min(cscores)
  S <- vector("numeric", length(rnks))
  S[cscores>0] <- cscores[cscores>0]/p
  S[cscores<0] <- -cscores[cscores<0]/q

  rind <- seq(along=rnks)
  ind <- rind[order(S, ksups, decreasing=TRUE)]

  ## permutation test
  
  res <- data.frame(index=ind,
                    cScore=S[ind],
                    raw.cScore=cscores[ind],
                    ksup=ksups[ind],
                    ksdown=ksdowns[ind])
  return(res)
}



