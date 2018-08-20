library(ribiosCGI)

geneids <- c(7350, 7351)
genes <- c("UCP1", "UCP2")
geneInts <- ronetGGI(geneids, mode="between")
expect_true(with(geneInts, all(GeneSymbol1 %in% genes)))
expect_true(with(geneInts, all(GeneSymbol2 %in% genes)))

ronetGGIcols <- c("RonetID", "Gene1", "GeneSymbol1", "InteractionType",
                  "InteractionTypeDesc", "Effect", "Gene2", "GeneSymbol2",
                  "Description","Source", "Xref")
expect_equivalent(sort(colnames(geneInts)), sort(ronetGGIcols))
