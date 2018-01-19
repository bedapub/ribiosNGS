## Test colKappa

testMat <- cbind(c(1,1,0,0,1,0), c(1,1,0,1,1,0))
testMatExp <- matrix(c(1,2/3, 2/3, 1), byrow=TRUE, nrow=2)

testMatLAkappa <- colKappa(testMat)
expect_that(testMatExp, equals(testMatLAkappa))

