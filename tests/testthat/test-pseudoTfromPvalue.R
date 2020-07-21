library(ribiosNGS)
library(testthat)

set.seed(1887)
t1 <- rnorm(10, 3)
t2 <- rnorm(10, 5)
ttestRes <- t.test(t1, t2)

test_that("pseudoTfromPValue works fine with an result of t test", {
  pseudoT <- pseudoTfromPvalue(ttestRes$p.value, ttestRes$parameter["df"],
                               sign=sign(mean(ttestRes$conf.int)))
  testthat::expect_equivalent(pseudoT, ttestRes$statistic)
  
  pseudoTval <- pseudoTfromPvalue(ttestRes$p.value, ttestRes$parameter["df"],
                               sign=mean(ttestRes$conf.int))
  testthat::expect_equivalent(pseudoTval, ttestRes$statistic)
})
