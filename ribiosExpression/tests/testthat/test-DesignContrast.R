# Test functions about design and contrast matrix

library(ribiosExpression)
library(ribiosIO)

infile <- function(file) system.file(file.path("extdata", file), package="ribiosExpression")
designFile <- infile("example-designMatrix.txt")
contrastFile <- infile("example-contrastMatrix.txt")
exprsFile <- infile("example-expression.gct")

exprsMat <- read_exprs_matrix(exprsFile)

descon <- parseDesignContrast(designFile=designFile,
                              contrastFile=contrastFile,
                              expSampleNames=colnames(exprsMat))

expect_identical(designMatrix(descon), readMatrix(designFile))
expect_identical(contrastMatrix(descon), readMatrix(contrastFile))
