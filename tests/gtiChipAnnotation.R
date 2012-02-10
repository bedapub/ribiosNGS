library(ribiosAnnotation)

supported.chips <- gtiChiptypes() 
stopifnot(is.character(supported.chips) && length(supported.chips)>1)
## "house-keeping" chips  probably are there
hkChips <- c("HG-U133_PLUS_2", "HG_U95A", "HUMANHT-12_V3_0_R1_11283641_A") 
stopifnot(all(hkChips %in% supported.chips))

stopifnot(all(isGtiChiptype(hkChips)))

## extHk
ehkChips <- c(hkChips, "Alster", "Elbe")
stopifnot(identical(isGtiChiptype(ehkChips),
                    c(rep(TRUE, length(hkChips)), FALSE, FALSE)))
stopifnot(all(isGtiChiptype(ehkChips, exceptions=c("Alster", "Elbe"))))
stopifnot(all(!isGtiChiptype(tolower(ehkChips), exceptions=c("Alster", "Elbe"))))
stopifnot(all(isGtiChiptype(tolower(ehkChips), exceptions=c("Alster", "Elbe"), ignore.case=TRUE)))


mychip <- "HG-U133_PLUS_2"
mychip.anno <- gtiChipAnnotation(mychip)
stopifnot(all(c("ProbeID", "GeneID", "GeneSymbol", "GeneName", "isSingleGeneID", "Chip") %in% colnames(mychip.anno)))
stopifnot(any(!is.na(mychip.anno$ProbeID)))
mychip.reanno <- annotateProbesets(sample(mychip.anno$ProbeID, 20),
                                   mychip)


## time tortouring to use IN syntax in querying chip annotation
torture <- FALSE
if(torture) {
  set.seed(1887)
  hgc.time <- system.time(hgc <- gtiChipAnnotation(chip="HG-U133_PLUS_2"))
  gc.no <- c(5, 10, 20, 50,100, 250,500,750,1000)
  N <- 16
  gc.sample <- lapply(gc.no, function(x) lapply(1:N, function(i) sample(hgc$GeneID, x)))
  gc.times <- lapply(gc.sample, function(x) sapply(x, function(y) system.time(gtiChipAnnotation(chip="HG-U133_PLUS_2", inCol="GeneID", inVal=y))))
  gcTime <- lapply(gc.times, function(x) x[3,])
  gcTimeMean <-  sapply(gcTime, mean)
  gcTimeSd <-  sapply(gcTime, sd)
  plot(gc.no, gcTimeMean, type="l",  log="y", ylim=c(0.05, 1),
       xlab="Query signature size",
       ylab="Total query elapsing time [s]")
  points(gc.no, gcTimeMean, pch=16)
  arrows(gc.no, gcTimeMean-gcTimeSd,
         gc.no, gcTimeMean+gcTimeSd,
         angle=90, length=0.05, code=3L)
  abline(h=hgc.time[3], col="red", lty=2L, lwd=1.5)
  text(800, hgc.time[3], "Without optimization", pos=3)
  text(800, gcTimeMean[length(gc.no)], "With optimization", pos=3)
}
