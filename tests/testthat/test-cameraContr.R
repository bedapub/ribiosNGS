library(limma)
library(ribiosNGS)

set.seed(1887)
y <- matrix(rnorm(1000*6),1000,6)
rownames(y) <- sprintf("Gene%d", 1:nrow(y))
design <- cbind(Intercept=1,Group=c(0,0,0,1,1,1))

## index1: genuinely significantly up-regulated
index1 <- 1:20
y[index1,4:6] <- y[index1,4:6]+1
## index2: not DEs
index2 <- 21:40

gs <- list(GeneSet1=index1, GeneSet2=index2)
(cameraRes <- camera(y, gs, design))
(cameraModRes <- cameraContr(y, gs, design))
stopifnot(ncol(cameraModRes)==6)
stopifnot(identical(cameraRes[,1:5], cameraModRes[,1:5]))

## test no row names
yNoRowNames <- y
rownames(yNoRowNames) <- NULL
(cameraModRes.noRowNames <- cameraContr(yNoRowNames, gs, design))
stopifnot(identical(cameraRes[,1:5], cameraModRes.noRowNames[,1:5]))

## test rankSum test
(cameraRsRes <- camera(y, gs, design, use.ranks=TRUE))
(cameraRsModRes <- cameraContr(y, gs, design, use.ranks=TRUE))
stopifnot(ncol(cameraRsModRes)==6)
stopifnot(identical(cameraRsRes[,1:5], cameraRsModRes[,1:5]))
