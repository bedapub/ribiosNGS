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
