humanRefSeq <- mRNArefseqByTaxID(9606)

context("Test mRNArefseqByTaxID")
expect_identical(colnames(humanRefSeq),
                 c("RefSeq", "GeneID", "GeneSymbol",
                   "Description", "GeneType"))

## cherry picking examples of MAPK14
expect_true("NM_001315" %in% humanRefSeq$RefSeq)
expect_true(1432 %in% humanRefSeq$GeneID)
