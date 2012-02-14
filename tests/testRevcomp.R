library(ribiosDemo)
##dyn.load("/apps/bi/apps/ribios/ribiosDemo/src/bios_revcomp.so")


revcomp("ATGC")
revcomp(c("ATGC", "CGTA"))
revcomp(c("AUGCUUAUA", "UAUUGCUUGCGUA"))

revcompNative("ATGC")
revcompNative(c("ATGC", "CGTA"))
revcompNative(c("AUGCUUAUA", "UAUUGCUUGCGUA"))

simpleString <- c("ATGCTAGCGGTCTATGCGCATGCATG")
stopifnot(identical(revcomp(simpleString), revcompNative(simpleString)))

longString <- makeSeq(len=1000)
longStringRC <- revcomp(longString)
longStringRCR <- revcompNative(longString)
stopifnot(identical(longStringRC, longStringRCR))

torture <- FALSE
if(torture) {
  seqlen <- c(1000,10000,50000, 100000, 1000000, 10000000)
  seqL <- length(seqlen)
  seqN <- 5
  seqs <- lapply(seq(along=seqlen),
                 function(x) sapply(1:seqN,
                                    function(y) makeSeq(len=seqlen[x])))
  revcomp.time <- sapply(seqs, function(x) system.time(revcomp(x)))
  revcompNative.time <- sapply(seqs, function(x) system.time(revcompNative(x)))
  revcomp.elapsed <- revcomp.time["elapsed",]/seqN
  revcompNative.elapsed <- revcompNative.time["elapsed",]/seqN
  plot(seqlen,revcompNative.elapsed,
       type="l", col="black", log="x",
       xlab="Sequence length [bp]", ylab="Elapsed time [s]",
       xaxt="n", bty="l")
  axis(1L, at=seqlen, las=3,
       labels=c("1k", "10k", "50k", "100k", "1Mb", "10Mb"))
  lines(seqlen, revcomp.elapsed,  type="l", col="red")
  points(seqlen, revcompNative.elapsed, col="black", pch=16)
  points(seqlen, revcomp.elapsed, col="red", pch=16)
  text(seqlen[seqL], revcomp.elapsed[seqL],
       format(revcomp.elapsed[seqL], digits=2), pos=3, col="red")
  text(seqlen[seqL], revcompNative.elapsed[seqL],
       format(revcompNative.elapsed[seqL], digits=3),
       pos=2, col="black")
  legend("topleft",
         c("revcomp using BIOS",
           "revcomp using R-native codes"),
         pch=16, lty=1L, col=c("red", "black"), bty="n")
}
