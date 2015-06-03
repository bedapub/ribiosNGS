## test read_exprs_matrix function
library(ribiosDemo)
##dyn.load("/apps/bi/apps/ribios/ribiosDemo/src/bios_readgct.so")

gctfile <- makeTempGct(nrow=50, ncol=50)
system.time(gctByBIOS <- readGct(gctfile))
system.time(gctByNative <- readGctNative(gctfile))
file.remove(gctfile)

torture <- FALSE
if(torture) {
  nsam <- c(10,50,100,200)
  nsamLen <- length(nsam)
  gctfiles <- sapply(nsam, function(x) makeTempGct(nrow=22000, ncol=x))
  readGctTimes <- sapply(gctfiles, function(f) system.time(readGct(f)))
  readGctNativeTimes <- sapply(gctfiles, function(f) system.time(readGctNative(f)))
  readGct.elapsed <- readGctTimes["elapsed",]
  readGctNative.elapsed <- readGctNativeTimes["elapsed",]
  par(mar=c(3.5,3.5,1,1),mgp=c(2,1,0))
  plot(nsam, readGctNative.elapsed, type="l",
       xlab="Sample Number (# feature=22000)", ylab="Elapsed time [s]",
       bty="l", xaxt="n")
  axis(1L, at=nsam, labels=nsam)
  lines(nsam, readGct.elapsed, type="l", col="red")
  points(nsam, readGctNative.elapsed, pch=16, col="black")
  points(nsam, readGct.elapsed, pch=16, col="red")
  text(nsam[nsamLen], readGct.elapsed[nsamLen],
       format(readGct.elapsed[nsamLen],digits=3L),
       col="red", pos=3L)
  text(nsam[nsamLen], readGctNative.elapsed[nsamLen],
       format(readGctNative.elapsed[nsamLen],digits=4L),
       col="black", pos=2L)
  legend("topleft",
         c("read GCT file with BIOS",
           "read GCT file with native R codes"),
         pch=16, lty=1L, col=c("red", "black"), bty="n")
  sapply(gctfiles, file.remove)
}
