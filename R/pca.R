getPCAlist <- function(pca) {
  list(expVar=pca$sdev^2/sum(pca$sdev^2),
       pc1_3=pca$x[,1:pmax(3, ncol(pca$x)),drop=FALSE])
}
getOrdList <- function(ord) {
  list(eig=ord$ord$eig,
       co=ord$ord$co)
}
