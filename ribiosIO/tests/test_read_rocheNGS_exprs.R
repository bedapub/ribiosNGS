library(ribiosIO)

ngsFile <- system.file("extdata/NGS_exprs/file1.expression", package="ribiosIO")
ngs <- read_rocheNGS_exprs(ngsFile)

## mandatory columns
stopifnot(identical(colnames(ngs)[1:5],
                    c("RPKM_MultiMap", "ReadCount_MultiMap", "RPKM_UniqMap", "ReadCount_UniqMap", "MultiProp")))

## rownames
stopifnot(identical(rownames(ngs)[1:3],
                    c("ENSG00000234128_7","ENSG00000172653_17", "ENSG00000181092_3")))
