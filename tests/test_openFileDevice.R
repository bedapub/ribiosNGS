library(ribiosUtils)

tempfile1 <- paste(tempfile(), ".pdf", sep="")
openFileDevice(tempfile1)
plot(rnorm(100), rnorm(100))
dev.off()

tempfile2 <- paste(tempfile(), ".png", sep="")
openFileDevice(tempfile2, width=5, height=5)
plot(rnorm(100), rnorm(100))
dev.off()

file.remove(tempfile1, tempfile2)

torture <- FALSE
if(torture)  {
  tempfile3 <- paste(tempfile(), ".tiff", sep="")
  openFileDevice(tempfile3, width=5, height=5)
  plot(rnorm(100), rnorm(100))
  dev.off()
  
  tempfile3.var <- paste(tempfile(), ".tif", sep="")
  openFileDevice(tempfile3.var, width=5, height=5)
  plot(rnorm(100), rnorm(100))
  dev.off()
  
  tempfile4 <- paste(tempfile(), ".bmp", sep="")
  openFileDevice(tempfile4, width=5, height=5)
  plot(rnorm(100), rnorm(100))
  dev.off()
  
  tempfile5 <- paste(tempfile(), ".jpg", sep="")
  openFileDevice(tempfile5, width=5, height=5)
  plot(rnorm(100), rnorm(100))
  dev.off()
  
  tempfile5.var <- paste(tempfile(), ".jpeg", sep="")
  openFileDevice(tempfile5.var, width=5, height=5)
  plot(rnorm(100), rnorm(100))
  dev.off()

  file.remove(tempfile3, tempfile3.var, tempfile4, tempfile5, tempfile5.var)
}
