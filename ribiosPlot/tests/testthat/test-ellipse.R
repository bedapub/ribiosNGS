library(ribiosPlot)


expect_identical(degree2radian(180), pi)
expect_identical(radian2degree(0.5*pi), 90)

testX <- c(3,5,9,12,14)
testY <- c(4,7,8,13,19)
chisqConf <- 0.98
testMeans <- c(mean(testX), mean(testY))
testCov <- cov(matrix(c(testX, testY),byrow=FALSE, ncol=2))
testEigen <- eigen(testCov)
testOrient <- atan(testEigen$vectors[2,1]/testEigen$vectors[1,1])
testChisqCrit <- qchisq(chisqConf, df=2L)
if(testOrient<0) testOrient <- testOrient+2*pi

testConfEllipsePars <- ribiosPlot:::confEllipseParams(testX, testY, conf=0.98)
expect_identical(testConfEllipsePars$mean, testMeans)
expect_identical(testConfEllipsePars$a, sqrt(testChisqCrit*testEigen$values[1]))
expect_identical(testConfEllipsePars$b, sqrt(testChisqCrit*testEigen$values[2]))
expect_identical(testConfEllipsePars$alpha, testOrient)
