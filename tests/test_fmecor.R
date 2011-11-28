library(ribiosUtils)
set.seed(1887); fN<- 50
test.matrix <- matrix(rnorm(fN*fN, mean=2, sd=5), nrow=fN)
system.time(test.cor <- fmecor(test.matrix))
system.time(test.cor1 <- cor(t(test.matrix)))
