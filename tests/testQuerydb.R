library(ribiosAnnotation)

hc <- querydb("SELECT * FROM genome_sequence WHERE DB='HUMANN'", db="bin", user="genome", password="genome")
stopifnot(all(paste("CHR", 1:22, sep="") %in% hc$SEQ))

hcIn <- querydbSelectIn("SELECT * FROM genome_sequence WHERE DB='HUMANN' AND ",
                        inCol="SEQ", inValues=c("CHR1", "CHR5", "CHRX"),
                        db="bin", user="genome", password="genome")

hcIn <- querydbTmpTbl("SELECT * FROM genome_sequence WHERE DB='HUMANN' AND ",
                      inCol="SEQ", inValues=c("CHR1", "CHR5", "CHRX"),
                      db="bin", user="genome", password="genome")


## annotate probesets
library(ribiosAnnotation)
ORACLE.BIN.PWD <- ORACLE.BIN.USER <- "genome"
notValid <- ribiosAnnotation:::notValid
RIBIOS_TMP_TBL <- "RIBIOS_ID_TMP"
system.time(myProbesets <- gtiChipAnnotation(chip="HG-U133_PLUS_2"))

system.time(usingProbesets <- gtiChipAnnotation(chip="HG-U133_PLUS_2",
                                                ids=myProbesets$ProbeID))
system.time(freeProbesets <- annotateAnyProbeset(ids=myProbesets$ProbeID[1:100]))
system.time(freeProbesetsAll <- annotateAnyProbeset(ids=myProbesets$ProbeID))
system.time(freeProbesetsSomeOrtho <- annotateAnyProbeset(ids=myProbesets$ProbeID[1:1000],
                                                          orthologue=TRUE))

## Try to guess chip types
system.time(guessedTypes <- probesetsChiptype(myProbesets$ProbeID, sample=200))
system.time(guessedTypesAgain <- probesetsChiptype(myProbesets$ProbeID[1:150], sample=200))
system.time(bestGuessedType <- probesetsChiptype(myProbesets$ProbeID, maxOnly=TRUE))

system.time(myProbesets2 <- gtiChipAnnotation(chip="RAT230_2"))
system.time(guessedTypes2 <- probesetsChiptype(myProbesets2$ProbeID, maxOnly=FALSE, sample=250))
## 200genes/s for orthologue mapping
##system.time(freeProbesetsAllOrtho <- annotateAnyProbeset(ids=myProbesets$ProbeID,
##                                                         orthologue=TRUE))
