library(ribiosIO)

ngsFile <- system.file("extdata/NGS_exprs/file1.expression", package="ribiosIO")
ngs <- read_biokit_exprs(ngsFile)

## nrows must be correct
stopifnot(nrow(ngs)==20786)

## mandatory columns
stopifnot(identical(colnames(ngs)[1:6],
                    c("RPKM_MultiMap", "ReadCount_MultiMap", "RPKM_UniqMap", "ReadCount_UniqMap", "MultiProp", "AllMappingReads")))

## rownames
stopifnot(identical(rownames(ngs)[1:3],
                    c("101930417", "101929892", "101927617")))
