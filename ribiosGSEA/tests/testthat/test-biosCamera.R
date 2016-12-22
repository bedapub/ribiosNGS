library(limma)
library(ribiosGSEA)

set.seed(1887)
y <- matrix(rnorm(1000*6),1000,6)
rownames(y) <- sprintf("Gene%d", 1:nrow(y))
design <- cbind(Intercept=1,Group=c(0,0,0,1,1,1))

## index1: genuinely significantly up-regulated
index1 <- 1:20
y[index1,4:6] <- y[index1,4:6]+1
## index2: not DEs
index2 <- 21:40

## TODO: limma::camera has been updated. biosCamera has to be updated, too!

gs <- list(GeneSet1=index1, GeneSet2=index2)
(cameraRes <- camera(y, gs, design, inter.gene.cor=NA))
(cameraModRes <- biosCamera(y, gs, design))
addCols <- c("GeneSet", "Score", "ContributingGenes")
idCols <- c("NGenes", "Correlation", "Direction", "PValue", "FDR")
rownames(cameraRes) <- NULL
stopifnot(identical(cameraRes[,idCols], cameraModRes[,idCols]))
stopifnot(all(c(addCols, idCols) %in% colnames(cameraModRes)))

## test no row names
yNoRowNames <- y
rownames(yNoRowNames) <- NULL
(cameraModRes.noRowNames <- biosCamera(yNoRowNames, gs, design))
stopifnot(identical(cameraRes[,idCols], cameraModRes.noRowNames[,idCols]))

## test rankSum test
(cameraRsRes <- camera(y, gs, design, use.ranks=TRUE))
(cameraRsModRes <- biosCamera(y, gs, design, use.ranks=TRUE))
rownames(cameraRsRes) <- NULL
stopifnot(identical(cameraRsRes[,idCols], cameraRsModRes[,idCols]))
