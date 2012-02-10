library(ribiosAnnotation)

myGeneIDs <- c(1,1234,1432, 245908)
mappedProbesets(myGeneIDs, chip="HG-U133_PLUS_2")
mappedProbesets(myGeneIDs, chip="HG-U133_PLUS_2", isSingleGeneID=TRUE)
mappedProbesets(myGeneIDs, chip="HG-U133_PLUS_2", isSingleGeneID=TRUE, unlist=TRUE)
