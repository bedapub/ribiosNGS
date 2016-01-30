library(ribiosNGS)
library(ribiosGSEA)

mat <- matrix(c(10,3,5,9,3,5,
                2,4,8,12,9,9,
                3,5,7,5,4,4,
                3,3,12,12,0,1), ncol=6, byrow=TRUE)
rownames(mat) <- sprintf("gene%d", 1:nrow(mat))
designMatrix <- matrix(c(rep(1,6), c(0,0,1,1,0,0), c(0,0,0,0,1,1)),
                       byrow=FALSE, ncol=3)
contrastMatrix <- matrix(c(0,1,0,0,0,1), ncol=2, byrow=FALSE)
descon <- DesignContrast(designMatrix, contrastMatrix)

obj <- EdgeObject(mat,descon)

## NOTE THAT obj must have GeneSymbol in its fData
obj@dgeList$genes <- data.frame(GeneSymbol=rownames(mat))

gs1 <- GeneSet("DefaultCategory", "GeneSet1", "", c("gene1", "gene3"))
gs2 <- GeneSet("DefaultCategory", "GeneSet2", "", c("gene2", "gene4"))
gs3 <- GeneSet("DefaultCategory", "GeneSet3", "", c("gene1", "gene4"))
gsc <- GeneSets(gs1, gs2, gs3)
gsInd <- lapply(gsGenes(gsc), function(x) match(x, rownames(mat)))

voomCameraOut <- voomCamera(obj, gsc)

voomCameraTbl <- voomCameraOut@enrichTables

## validate
matVoom <- as.matrix(voom(mat))
resContrast1 <- camera(matVoom, gsInd, design=designMatrix, contrast=contrastMatrix[,1])
resContrast2 <- camera(matVoom, gsInd, design=designMatrix, contrast=contrastMatrix[,2])

expect_identical_field <- function(field) {
    expect_identical(c(resContrast1[, field],resContrast2[,field]),
                     voomCameraTbl[,field])
}

expect_identical_field("NGenes")
expect_identical_field("PValue")
expect_identical_field("FDR")
expect_identical_field("Correlation")
expect_identical_field("Direction")
expect_identical(c(rownames(resContrast1),rownames(resContrast2)),
                   voomCameraTbl$GeneSet)
