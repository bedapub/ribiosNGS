library(ribiosMath)
library(testthat)

synData <- matrix(c(rep(c(rep(1, 10), rep(0, 5)), 3),
                    rep(0, 4), rep(1, 7), rep(0,4),
                    rep(c(rep(0,5), rep(1,10)), 3),
                    rep(c(rep(0,3), 1), 4)[-16]), ncol=15, byrow=TRUE)
rownames(synData) <- sprintf("Gene %s", letters[1:8])
colnames(synData) <- sprintf("t%d", 1:15)
synKappaMat <- rowKappa(synData)
synKappaMat.round2 <- round(synKappaMat, 2)
synKappaMatTp <- colKappa(synData)
synKappaMatTp.round2 <- round(synKappaMatTp, 2)


Rout1 <- ribiosMath:::davidClustering_kappa_R(synKappaMat.round2, removeRedundant = TRUE, debug=FALSE)
Rout2 <- ribiosMath:::davidClustering_kappa_R(synKappaMatTp.round2, removeRedundant = TRUE, debug=FALSE)

Cppout1 <- davidClustering_kappa(synKappaMat.round2, kappaThr=0.35, mergeRule=3)
Cppout2 <- davidClustering_kappa(synKappaMatTp.round2, kappaThr=0.35, mergeRule=3)

expect_identical(Rout1, Cppout1)
expect_identical(Rout2, Cppout2)


## large matrix
set.seed(1887)
largeMat <- matrix(rbinom(10000, 1, 0.25), nrow=1000)
largematKappa <- round(rowKappa(largeMat),2)
system.time(rres <- davidClustering_kappa_R(largematKappa))
system.time(cppres <- davidClustering_kappa(largematKappa, mergeRule=3))

## note that now the seeds from R and from C++ have different orders
sortedSeeds <- function(x) x[order(sapply(x, paste, collapse=""))]
expect_identical(sortedSeeds(rres), sortedSeeds(cppres))