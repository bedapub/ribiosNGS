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

connScore <- function(rnk, up, down) {
  n <- length(rnk)
  vup <- match(up, rnk)
  vdown <- match(down,rnk)
  indexConnScore(n=n, ind.up=vup, ind.down=vdown)
}

connScorePerm <- function(rnk, up, down, B=1000) {
  n <- length(rnk)
  n.up <- length(up)
  n.down <- length(down)
  indexConnScorePerm(n=n, n.up=n.up, n.down=n.down, B=B)
}
