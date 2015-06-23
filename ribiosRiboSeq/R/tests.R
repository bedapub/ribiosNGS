library(ribiosUtils)
library(limma)
set.seed(1887)
stats <- rnorm(100000)
inds <- lapply(1:50, function(x)
               sample(length(stats), sample(3:500, 1)))

geneSetPerm <- function (statistics, ind.list, alternative = "mixed", type = "auto", 
                         nsim = 9999, func=median) {
  alternative <- match.arg(alternative, c("mixed", "either", 
                                          "down", "up", "less", "greater", "two.sided"))
  if (alternative == "two.sided") 
    alternative <- "either"
  if (alternative == "less") 
    alternative <- "down"
  if (alternative == "greater") 
    alternative <- "up"
  type <- match.arg(tolower(type), c("auto", "t", "f"))
  allsamesign <- all(statistics >= 0) || all(statistics <= 0)
  if (type == "auto") {
    if (allsamesign) 
      type <- "f"
    else type <- "t"
  }
  if (type == "f" & alternative != "mixed") 
    stop("Only alternative=\"mixed\" is possible with F-like statistics.")
  if (alternative == "mixed") 
    statistics <- abs(statistics)
  if (alternative == "down") {
    statistics <- -statistics
    alternative <- "up"
  }

  ## assume no NA in statistics or ind.list items
  ps <- rep(1L, length(ind.list))
  lengths <- sapply(ind.list, length)
  is.pos.length <- lengths>0
  
  list.stats <- sapply(ind.list, function(i)
                       do.call(func, list(statistics[i])))
  perms <- sapply(1:nsim, function(x) {
    pstats <- sample(statistics)
    plist.stats <- sapply(ind.list, function(i)
                          do.call(func, list(pstats[i])))
    return(plist.stats)
  })
  ps <- sapply(seq(along=ind.list), function(x) sum(list.stats[x]<=perms[x,]))
  permP <- (ps+1)/(nsim+1) 
  return(permP)
}

Rprof(filename="~/sandbox/Rprof.out", interval=0.001)
system.time(mt <- sapply(inds, function(x)
                         geneSetMedianTest(x, stats, alternative="mixed", ranks.only=FALSE)))
system.time(mt.mean <- sapply(inds, function(x)
                         geneSetTest(x, stats, alternative="mixed", ranks.only=FALSE)))

Rprof(NULL)

Rprof(filename="~/sandbox/RprofPerm.out", interval=0.001)
system.time(pmt <- geneSetPerm(stats, inds, alternative="mixed"))
Rprof(NULL)

system.time(pmt.mean <- geneSetPerm(stats, inds, alternative="mixed", func=mean))

stopifnot(cor(mt, pmt)>0.9)
