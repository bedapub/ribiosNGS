## tests
library(ribiosDiwa)

regN <- 5
deN <- 20
samX <- 15
samY <- 15

ERR <- 1E-8
set.seed(100)
reg1 <- matrix(rnorm(regN*samX), nrow=regN, byrow=TRUE)
reg2 <- matrix(rnorm(regN*samY)+1, nrow=regN, byrow=TRUE)
de1 <- matrix(rnorm(deN*samX), nrow=deN, byrow=TRUE)
de2 <- matrix(rnorm(deN*samY)+0.5, nrow=deN, byrow=TRUE)

## matrix-based method
system.time(rif1Res <- rif1Mat(reg1, reg2, de1, de2))
## vector-based method
system.time(rif1VecRes <- sapply(1:nrow(reg1), function(i)
                                 mean(sapply(1:nrow(de1), function(j)
                                             rif1Vec(reg1[i,], reg2[i,],
                                                     de1[j,], de2[j,])))))

stopifnot(all(abs(rif1Res-rif1VecRes)<ERR))
system.time(rif2Res <- rif2Mat(reg1, reg2, de1, de2))
system.time(rif2VecRes <- sapply(1:nrow(reg1), function(i)
                                 mean(sapply(1:nrow(de1), function(j)
                                             rif2Vec(reg1[i,], reg2[i,],
                                                     de1[j,], de2[j,])))))

stopifnot(all(abs(rif2Res-rif2VecRes)<ERR))
