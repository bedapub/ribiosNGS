library(ribiosPlot)

## helper functions
test <- c(2,4,3,-1,9,5,3,4)
bound(test,0,8)
boundNorm(test)

mat <- matrix(test, nrow=2)
bound(mat, 0, 8)
boundNorm(mat)
boundNorm(mat,0,8)

colorpanel(100, "white", "black", "green")

myMat <- matrix(rnorm(256), nrow=16)
myWH <- guessWH(nrow=nrow(myMat), ncol=ncol(myMat),
                xlab="321", ylab="ABC",
                rownames=rownames(myMat), colnames=colnames(myMat))

if(interactive()) {
  X11(width=myWH$width, height=myWH$height)
  biosHeatmap(myMat, lwid=myWH$lwid, lhei=myWH$lhei, xlab="321",
              ylab="ABC", cexRow=2L, cexCol=2L)
  dev.off()
}
