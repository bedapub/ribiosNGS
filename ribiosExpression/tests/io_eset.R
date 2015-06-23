library(Biobase)
library(ribiosExpression)
data(sample.ExpressionSet)

expfile <- tempfile()
ffile <- tempfile()
pfile <- tempfile()

writeEset(sample.ExpressionSet,
          exprs.file=expfile,
          fData.file=ffile,
          pData.file=pfile)
writeEset(sample.ExpressionSet,
          exprs.file=expfile)

fiIdentical <- function(x,y) identical(sapply(x, as.character), sapply(y, as.character))
##eset.full <- readEset(exprs.file=expfile,
##                      fData.file=ffile,
##                      pData.file=pfile)
##stopifnot(fiIdentical(fData(eset.full), fData(sample.ExpressionSet)))
##stopifnot(fiIdentical(pData(eset.full), pData(sample.ExpressionSet)))
##stopifnot(identical(exprs(eset.full), exprs(sample.ExpressionSet)))
##eset.fonly <- readEset(exprs.file=expfile,
##                       fData.file=ffile)
##eset.ponly <- readEset(exprs.file=expfile,
##                       pData.file=pfile)
##eset.minimum <- readEset(exprs.file=expfile)
##
##
##sysdir <- system.file("extdata", package="ribiosExpression")
##sysexp <- file.path(sysdir, "sample_eset_exprs.txt")
##sysfd <- file.path(sysdir, "sample_eset_fdata.txt")
##syspd <- file.path(sysdir, "sample_eset_pdata.txt")
##
##sys.eset <- readEset(exprs.file=sysexp,
##                     fData.file=sysfd,
##                     pData.file=syspd)
##stopifnot(identical(1:nrow(sys.eset),
##                    match(rownames(fData(sys.eset)) , rownames(read.table(sysfd)))))
##stopifnot(identical(1:ncol(sys.eset),
##                    match(rownames(pData(sys.eset)), rownames(read.table(syspd)))))
##sys.ponly <- readEset(exprs.file=sysexp,
##                      fData.file=NULL,
##                      pData.file=syspd)
##sys.fonly <- readEset(exprs.file=sysexp,
##                      fData.file=sysfd,
##                      pData.file=NULL)
##sys.minimum <- readEset(exprs.file=sysexp,
##                        fData.file=NULL,
##                        pData.file=NULL)
##
#### readEset is able to deal with number-only column names
##nset <- sample.ExpressionSet
##sampleNames(sample.ExpressionSet) <- 1:dim(sample.ExpressionSet)[2]
##writeEset(sample.ExpressionSet,
##          exprs.file=expfile,
##          fData.file=ffile,
##          pData.file=pfile)
##readEset(exprs.file=expfile,
##         fData.file=ffile,
##         pData.file=pfile)
