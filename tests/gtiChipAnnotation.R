library(ribiosAnnotation)

supported.chips <- gtiChiptypes() 
stopifnot(is.character(supported.chips) && length(supported.chips)>1)
## "house-keeping" chips  probably are there
stopifnot(all(c("HG-U133_PLUS_2", "HG_U95A", "HUMANHT-12_V3_0_R1_11283641_A") %in% supported.chips))

mychip <- "HG-U133_PLUS_2"
mychip.anno <- gtiChipAnnotation(mychip)
mychip.reanno <- annotateProbesets(sample(mychip.anno$ProbeID, 20),
                                   mychip)
