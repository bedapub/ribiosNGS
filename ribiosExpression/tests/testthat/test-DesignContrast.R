# Test functions about design and contrast matrix

library(ribiosExpression)
library(ribiosIO)

infile <- function(file) file.path("../inst/extdata", file)
designFile <- infile("example-DesignMatrix.txt")
contrastFile <- infile("example-ContrastMatrix.txt")
exprsFile <- infile("example-expression.gct")

exprsMat <- read_exprs_matrix(exprsFile)

descon <- parseDesignContrast(designFile=designFile,
                              contrastFile=contrastFile,
                              expSampleNames=colnames(exprsMat))

expect_identical(designMatrix(descon), readMatrix(designFile))
expect_identical(contrastMatrix(descon), readMatrix(contrastFile))
