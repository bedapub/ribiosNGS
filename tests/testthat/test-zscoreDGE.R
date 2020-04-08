library(edgeR)
library(ribiosNGS)

## make sure that zscoreDGE and .zscoreDGE in edgeR (not exported) are identical
## TODO: better to check consistency outcome
## expect_equal(zscoreDGE, edgeR:::.zscoreDGE)
