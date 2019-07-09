library(ribiosNGS)
library(ribiosGSEA)
library(ribiosUtils)
library(BioQC)
library(testthat)

mat <- matrix(c(10,3,5,9,3,5,
                2,4,8,12,9,9,
                3,5,7,5,4,4,
                3,3,12,12,0,1), ncol=6, byrow=TRUE)
rownames(mat) <- sprintf("gene%d", 1:nrow(mat))
designMatrix <- matrix(c(rep(1,6), c(0,0,1,1,0,0), c(0,0,0,0,1,1)),
                       byrow=FALSE, ncol=3)
contrastMatrix <- matrix(c(0,1,0,0,0,1), ncol=2, byrow=FALSE)
descon <- DesignContrast(designMatrix, contrastMatrix)

obj <- EdgeObject(mat,descon, fData=data.frame(GeneSymbol=rownames(mat)))

## NOTE THAT obj must have GeneSymbol in its fData

gs1 <- list(name="GeneSet1", desc="", genes=c("gene1", "gene3"), namespace="default")
gs2 <- list(name="GeneSet2", desc="", genes=c("gene2", "gene4"), namespace="default")
gs3 <- list(name="GeneSet3", desc="", genes=c("gene1", "gene4"), namespace="default")
gmtlist <- GmtList(list(gs1, gs2, gs3))
gsInd <- matchGenes(gmtlist, rownames(mat))

voomCameraOut <- voomCamera(obj, gmtlist)

voomCameraTbl <- voomCameraOut@enrichTables

## validate
## Note that from limma 3.29.6, the default parameters of camera changed: inter.gene.cor=0.01 (used to be NA), and allow.neg.cor=FALSE (used to be TRUE)
matLibSizes <- colSums(mat) * calcNormFactors(mat)
matVoom <- as.matrix(voom(mat, design=designMatrix, lib.size=matLibSizes))
## notice that the biosCamera implementation may slightly differ from the limma::camera implementation
## resContrast1 <- camera(matVoom, gsInd, design=designMatrix, contrast=contrastMatrix[,1], inter.gene.cor=NA, allow.neg.cor=FALSE, sort=FALSE)
## resContrast2 <- camera(matVoom, gsInd, design=designMatrix, contrast=contrastMatrix[,2], inter.gene.cor=NA, allow.neg.cor=FALSE, sort=FALSE)
resContrast1 <- biosCamera(matVoom, gsInd, design=designMatrix, contrast=contrastMatrix[,1], allow.neg.cor=FALSE, sort=FALSE)
resContrast2 <- biosCamera(matVoom, gsInd, design=designMatrix, contrast=contrastMatrix[,2], allow.neg.cor=FALSE, sort=FALSE)

## make sure that the order is correct
voomCameraTbl <- with(voomCameraTbl, voomCameraTbl[order(Contrast, GeneSet),])

expect_identical_field <- function(field) {
    expect_identical(c(resContrast1[, field],resContrast2[,field]),
                     voomCameraTbl[,field])
}
expect_equal_field <- function(field) {
    expect_equal(c(resContrast1[, field],resContrast2[,field]),
                 tolerance=1E-7,
                     voomCameraTbl[,field])
}
expect_identical_field("NGenes")
expect_equal_field("PValue")
expect_equal_field("FDR")
expect_identical_field("Correlation")
expect_identical_field("Direction")
expect_identical(c(resContrast1$GeneSet,resContrast2$GeneSet),
                   voomCameraTbl$GeneSet)

