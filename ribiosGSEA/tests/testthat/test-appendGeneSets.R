library(ribiosGSEA)
library(testthat)

gs1 <- GeneSet(category="C1",
               name="Gene set 1",
               desc="test gene set",
               genes=c("ABCA1", "DDR1"))
gs2 <- GeneSet(category="C1",
               name="Gene set 2",
               desc="test gene set",
               genes=c("MAPK14", "MAP2K1"))
gs3 <- GeneSet(category="C2",
               name="Gene set 3",
               desc="test gene set",
               genes=c("JAK1", "JAK2"))
gs4 <- GeneSet(category="C3",
               name="Gene set 4",
               desc="test gene set",
               genes=c("XPO1", "SRC"))
gs5 <- GeneSet(category="C3",
               name="Gene set 5",
               desc="test gene set",
               genes=c("DDR2", "RAF1"))

gsets1 <- GeneSets(list(gs1, gs2))
gsets2 <- GeneSets(gs3)
gsets3 <- GeneSets(gs4, gs5)

gAppend <- appendGeneSets(gsets1, gsets2)
gAppend2 <- appendGeneSets(gsets1, gsets2, gsets3)

gAppendDirect <- GeneSets(gs1, gs2, gs3)
gAppend2Direct <- GeneSets(gs1, gs2, gs3, gs4, gs5)

expect_identical(gAppend, gAppendDirect)
expect_identical(gAppend2, gAppend2Direct)
