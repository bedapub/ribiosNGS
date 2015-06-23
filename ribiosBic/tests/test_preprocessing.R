library(ribiosBic)
library(ribiosExpression)

SDIR <- system.file("Rscripts", package="ribiosBic")
IDIR <- system.file("extdata", package="ribiosExpression")

tmpfile <- tempfile()
comm <- paste(file.path(SDIR, "bianchi_preprocess.Rscript"),
              "-infile", file.path(IDIR, "sample_eset_exprs.txt"),
              "-chiptype", "HG-U133_PLUS_2",
              "-outfile",tmpfile,
              "-summfeat", "true",
              "-pfile", file.path(IDIR, "sample_eset_pdata.txt"),
              "-ffile", file.path(IDIR, "sample_eset_fdata.txt"),
              collapse=" ")
system(comm)              
load(tmpfile)
print(eset)
