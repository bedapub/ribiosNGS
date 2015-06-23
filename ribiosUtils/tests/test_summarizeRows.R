library(ribiosUtils)
my.matrix <- matrix(1:36, nrow=6)
print(my.matrix)

my.factor <- factor(c("A", "B", "A", "C", "B", "B"))
rowm <- summarizeRows(matrix=my.matrix, factor=my.factor, fun=mean)
rowm.exp <-  matrix(c(2,8,14,20,26,32,13/3, 31/3, 49/3, 67/3, 85/3, 103/3, 4,10,16,22,28,34), nrow=3, byrow=TRUE, dimnames=list(c("A","B","C"),NULL))
stopifnot(identical(rowm,rowm.exp))

rowp <- summarizeRows(matrix=my.matrix, factor=my.factor, fun=prod)
rowp.exp <- matrix(c(3, 63, 195,399, 675,1023, 60, 1056, 4284, 11040, 22620, 40320, 4, 10,16,22,28,34), nrow=3, byrow=TRUE, dimnames=list(c("A","B","C"),NULL))
stopifnot(identical(rowp, rowp.exp))

colm <- summarizeColumns(matrix=my.matrix, factor=my.factor, fun=mean)
colm.exp <- matrix(as.numeric(c(7:12, 21:26, 19:24)), ncol=3, byrow=FALSE,dimnames=list(NULL, c("A", "B","C")))
stopifnot(identical(colm, colm.exp))

colp <- summarizeColumns(matrix=my.matrix, factor=my.factor, fun=prod)
colp.exp <- matrix(c(13,28,45,64,85,108, 5425, 6656, 8019, 9520, 11165,12960, 19,20,21,22,23,24), ncol=3, byrow=FALSE, dimnames=list(NULL, c("A", "B","C")))
stopifnot(identical(colp, colp.exp))

## NA values in factor
my.na.factor <- factor(c("A", "B", "A", "C", NA, "B"))
rowmn <- summarizeRows(matrix=my.matrix, factor=my.na.factor, fun=mean)
rowmn.exp <- matrix(c(2,8,14,20,26,32,4,10,16,22,28,34, 4,10,16,22,28,34), nrow=3, byrow=TRUE, dimnames=list(c("A","B","C"),NULL))
stopifnot(identical(rowmn, rowmn.exp))

rowpn <- summarizeRows(matrix=my.matrix, factor=my.na.factor, fun=prod)
rowpn.exp <- matrix(c(3, 63, 195,399, 675,1023, 12, 96, 252, 480, 780, 1152, 4, 10,16,22,28,34), nrow=3, byrow=TRUE, dimnames=list(c("A","B","C"),NULL))
stopifnot(identical(rowpn, rowpn.exp))

colmn <- summarizeColumns(matrix=my.matrix, factor=my.na.factor, fun=mean)
colmn.exp <- matrix(as.numeric(c(7:12, 19:24, 19:24)), ncol=3, byrow=FALSE,dimnames=list(NULL, c("A", "B","C")))
stopifnot(identical(colmn, colmn.exp))

colpn <- summarizeColumns(matrix=my.matrix, factor=my.na.factor, fun=prod)
colpn.exp <- matrix(c(13,28,45,64,85,108, 217, 256, 297, 340, 385,432, 19,20,21,22,23,24), ncol=3, byrow=FALSE, dimnames=list(NULL, c("A", "B","C")))
stopifnot(identical(colpn, colpn.exp))
