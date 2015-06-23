library(ribiosUtils)

set.seed(1887)
mat1 <- matrix(1:12, nrow=4L, ncol=3L,
               dimnames=list(c("A", "B", "C", "D"), c("HSV", "FCB", "BVB")))
mat1.longdf <- matrix2longdf(mat1)
stopifnot(identical(as.character(mat1.longdf$row),
                    rep(LETTERS[1:4], 3L)))
stopifnot(identical(as.character(mat1.longdf$column),
                    rep(c("HSV", "FCB", "BVB"), each=4L)))
stopifnot(identical(mat1.longdf$value, 1:12))

mat2 <- matrix(as.character(rnorm(100)),
               nrow=10, ncol=10)
mat2.longdf <- matrix2longdf(mat2)
mat2.longdf2 <- matrix2longdf(mat2, row.names=LETTERS[1:10], col.names=letters[1:10])
mat2.longdf3 <- matrix2longdf(mat2, row.names=LETTERS[1:10])
mat2.longdf4 <- matrix2longdf(mat2, col.names=letters[1:10])

## inconsistent length
suppressWarnings(mat2.longdf.incons <- matrix2longdf(mat2, col.names=letters[1:5]))
