library(ribiosIO)

ngsFile <- system.file("extdata/biokit_expression_files/biokit-output-1.expression", package="ribiosIO")
ngs <- read_biokit_exprs(ngsFile)

## nrows must be correct
stopifnot(nrow(ngs)==229)

## mandatory columns (name set in C routine)
stopifnot(identical(colnames(ngs)[1:6],
                    c("RPKM_MultiMap", "ReadCount_MultiMap", 
                      "RPKM_UniqMap", "ReadCount_UniqMap",
                      "MultiProp", "AllMappingReads")))

## rownames
stopifnot(identical(rownames(ngs)[1:3],
                    c("55423", "100616372", "5568")))
