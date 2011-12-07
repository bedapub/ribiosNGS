## test read_exprs_matrix function
library(ribiosIO)
testfile.path <- system.file("extdata", package="ribiosIO")

## import gct
input.matrix <- matrix(c(1,2,3,2,3,4,3,4,5), nrow=3, byrow=TRUE, dimnames=list(c("a", "b", "c"),c("a","b","c")))
input.gct <- read_exprs_matrix(file.path(testfile.path,"test_read_exprs_matrix.gct"))
storage.mode(input.matrix) <- "integer"
storage.mode(input.gct) <- "integer" ## make the comparison easier
stopifnot(identical(input.gct, input.matrix))

## import gmt
testgmt <- file.path(testfile.path, "test.gmt")
testgmt.list <- read_gmt_list(testgmt)

## import tab-separated file (without/with colname for the first column)
assertIdentical <- function(x,y) stopifnot(identical(x,y))
assertIdentical(read_exprs_matrix(file.path(testfile.path, "test_read_exprs_matrix.tsv")),
                input.matrix)
assertIdentical(read_exprs_matrix(file.path(testfile.path, "test_read_exprs_matrix_full.tsv")),
                input.matrix)

## import space-separated file (without/with colname for the first column)
assertIdentical(read_exprs_matrix(file.path(testfile.path, "test_read_exprs_matrix.txt")),
                input.matrix)
assertIdentical(read_exprs_matrix(file.path(testfile.path, "test_read_exprs_matrix_full.txt")),
                input.matrix)

## how to deal with multiple row names
## moaning with anyDuplicate(rnames) is a hard-coded feature of read.table

dup.mat <- matrix(c(1.1, 2.2, 3.3, 2.2, 3.3, 4.4, 3.3, 4.4, 5.5), nrow=3, byrow=TRUE, dimnames=list(c("a", "b", "a"), c("a", "b", "c")))
assertRoundIdentical <- function(x,y,digit=1L) stopifnot(identical(round(x,digit), round(y,digit)))
assertRoundIdentical(dup.mat,
                     read_exprs_matrix(file.path(testfile.path, "test_read_exprs_matrix_duprownames.tsv")))

assertRoundIdentical(dup.mat,
                     read_exprs_matrix(file.path(testfile.path, "test_read_exprs_matrix_duprownames.txt")))

assertRoundIdentical(dup.mat,
                     read_exprs_matrix(file.path(testfile.path, "test_read_exprs_matrix_full_duprownames.tsv")))
assertRoundIdentical(dup.mat,
                     read_exprs_matrix(file.path(testfile.path, "test_read_exprs_matrix_full_duprownames.txt")))
