#' @importFrom graphics plot points rug
#' @importFrom stats ecdf p.adjust

ksStat <- function(x,y) {
  x.dup <- sort(x)
  y.dup <- sort(y)
  .Call("kssorted", x.dup, y.dup)
}
ksBoot <- function(x, n, B) {
  x.dup <- sort(as.numeric(x))
  x.ind <- seq(along=x)
  stopifnot(B>1)
  inds <- 1:B
  y.ind <- sapply(inds, function(i)
                  x.dup[sort(sample(x.ind, n, replace=TRUE))])
  sapply(inds, function(i) {
    .Call("kssorted", x.dup, y.ind[,i])
  })
}

ecdf2 <- function(x,y) {
  plot(ecdf(x), verticals=TRUE)
  rug(x)
  plot(ecdf(y), verticals=TRUE, add=TRUE, col="red")
  rug(y, col="red")
  xy.inter <- intersect(x,y)
  if(length(xy.inter)>0)
    points(xy.inter,rep(0, length(xy.inter)),pch=4)
}

#' @export cmap
cmap <- function(rnks, up, down,
                 group,
                 permG=0,
                 sortBy=c("default", "none", "p")) {
  sortBy <- match.arg(sortBy)
  stopifnot(is.matrix(rnks))

  cscoreAll <- connScore(rnks, up, down)
  
  cscores <- cscoreAll[, 1L]
  ksups <- cscoreAll[, 2L]
  ksdowns <- cscoreAll[, 3L]
  
  ## permutation by group
  if(!missing(group) && permG>0) {
    cscore.bg <- split(cscores, group)
    nonnull.bg <- sapply(cscore.bg, function(x) mean(x!=0))
    mean.cscore.bg <- sapply(cscore.bg, mean)
    cscore.bg.len <- sapply(cscore.bg, length)
    p.bg <- p.bg.adj <- numeric(nlevels(group))

    p.bg.valid <- !(nonnull.bg < 0.50 | mean.cscore.bg == 0 | cscore.bg.len<=1L)
    p.bg[!p.bg.valid] <- p.bg.adj[!p.bg.valid] <- 1L

    ks.bg <- sapply(cscore.bg[p.bg.valid],
                    function(x) ksStat(x, cscores))
    
    ks.bg.len <- cscore.bg.len[p.bg.valid]
    uniq.nins <- setdiff(unique(sort(ks.bg.len)), 1L)
    pool <- sort(unique(cscores))

    perm.ks <- sapply(uniq.nins,
                      function(x) {ksBoot(pool, x, B=permG)})

    ks.bg.len.match <- match(ks.bg.len, uniq.nins)
    ks.bg.ps <- sapply(seq(along=ks.bg),
                       function(x)
                       mean(abs(ks.bg[x]) <= abs(perm.ks[, ks.bg.len.match[x] ])))
    
    ks.bg.ps.adj <- p.adjust(ks.bg.ps, "BH")
    p.bg[p.bg.valid] <- ks.bg.ps
    p.bg.adj[p.bg.valid] <- ks.bg.ps.adj

    inds <- as.integer(group)
    cscoreP <- p.bg[inds]
    cscoreFDR <- p.bg.adj[inds]
  } else {
    group <- cscoreP <- cscoreFDR <- rep(as.numeric(NA), length(cscores))
  }
     
  rind <- seq(along=cscores)
  S <- vector("numeric", length(cscores))
  
  ## scaling
  p <- max(cscores)
  q <- min(cscores)
  S[cscores>0] <- cscores[cscores>0]/p
  S[cscores<0] <- -cscores[cscores<0]/q
  if(!missing(group) && permG>0) {
    Sg <- tapply(S, group, mean)[inds]
  } else {
    Sg <- rep(NA, length(S))
  }

  ind <- rind[order(S, ksups, decreasing=FALSE)]
  res <- data.frame(index=ind,
                    connScore=S[ind],
                    raw.connScore=cscores[ind],
                    ksup=ksups[ind],
                    ksdown=ksdowns[ind],
                    group=group[ind],
                    groupMeanConnScore=Sg[ind],
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
