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

eset.full <- readEset(exprs.file=expfile,
                      fData.file=ffile,
                      pData.file=pfile)
eset.fonly <- readEset(exprs.file=expfile,
                       fData.file=ffile)
eset.ponly <- readEset(exprs.file=expfile,
                       pData.file=pfile)
eset.minimum <- readEset(exprs.file=expfile)


sysdir <- system.file("extdata", package="ribiosExpression")
sysexp <- file.path(sysdir, "sample_eset_exprs.txt")
sysfd <- file.path(sysdir, "sample_eset_fdata.txt")
syspd <- file.path(sysdir, "sample_eset_pdata.txt")

sys.eset <- readEset(exprs.file=sysexp,
                     fData.file=sysfd,
                     pData.file=syspd)
sys.ponly <- readEset(exprs.file=sysexp,
                      fData.file=NULL,
                      pData.file=syspd)
sys.fonly <- readEset(exprs.file=sysexp,
                      fData.file=sysfd,
                      pData.file=NULL)
sys.minimum <- readEset(exprs.file=sysexp,
                        fData.file=NULL,
                        pData.file=NULL)
