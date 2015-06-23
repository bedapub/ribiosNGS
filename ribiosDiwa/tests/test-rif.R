library(ribiosDiwa)
expMat <- matrix(rnorm(2000), nrow=200)
reg.ind <- sample(1:200, 50)
de.ind <- sample(1:200, 200)
fac <- gl(2, 5)
system.time(expRIF <- RIFscore(matrix=expMat, reg.ind=reg.ind, de.ind=de.ind,
                               fac=fac))
system.time(expRIF <- RIFscore(matrix=expMat, reg.ind=reg.ind, de.ind=de.ind,
                               fac=fac, permutation=9))
