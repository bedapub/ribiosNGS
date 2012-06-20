library(ribiosAnnotation)

hc <- querydb("SELECT * FROM genome_sequence WHERE DB='HUMANN'", db="bin", user="genome", password="genome")
stopifnot(all(paste("CHR", 1:22, sep="") %in% hc$SEQ))

hcIn <- querydbSelectIn("SELECT * FROM genome_sequence WHERE DB='HUMANN' AND ",
                        inCol="SEQ", inValues=c("CHR1", "CHR5", "CHRX"),
                        db="bin", user="genome", password="genome")

hcIn <- querydbTmpTbl("SELECT * FROM genome_sequence WHERE DB='HUMANN' ",
                      inCol="SEQ", inValues=c("CHR1", "CHR5", "CHRX"),
                      db="bin", user="genome", password="genome")


## annotate probesets
system.time(myProbesets <- gtiChipAnnotation(chip="HG-U133_PLUS_2"))

system.time(usingProbesets <- gtiChipAnnotation(chip="HG-U133_PLUS_2",
                                                ids=myProbesets$ProbeID))
system.time(probesetsNoOrth <- gtiChipAnnotation(chip="RA_CUSTOM_V1"))
system.time(probesetsOrth <- gtiChipAnnotation(chip="RA_CUSTOM_V1",
                                               orthologue=TRUE))
system.time(probesetsMultiOrth <- gtiChipAnnotation(chip="RA_CUSTOM_V1",
                                               orthologue=TRUE, multiOrth=TRUE))

system.time(largeProbesetsOrth <- gtiChipAnnotation(chip="RAT230_2",
                                                    orthologue=TRUE, multiOrth=FALSE))
## TODO: THE PROBLEM NOW IS MATCHCOLUMN (MULTI=TRUE)
## system.time(largeProbesetsOrthMulti <- gtiChipAnnotation(chip="RAT230_2",
##                                                         orthologue=TRUE, multiOrth=TRUE))
system.time(multiOrths <- gtiChipAnnotation(chip="RAT230_2",
                                            ids=c("1386637_at", "1392894_at", "1370623_at", "1383516_at"),
                                            orthologue=TRUE, multiOrth=TRUE))
system.time(singleOrths <- gtiChipAnnotation(chip="RAT230_2",
                                             ids=c("1386637_at", "1392894_at", "1370623_at", "1383516_at", "missing"),
                                             orthologue=TRUE, multiOrth=FALSE))


## annotateAnyProbesets
system.time(freeProbesets <- annotateAnyProbeset(ids=myProbesets$ProbeID[1:100]))
system.time(freeProbesetsAll <- annotateAnyProbeset(ids=myProbesets$ProbeID))
## orthologue mappings are NOT ANY MORE slow: query SQL twice made it faster
system.time(freeProbesetsSomeOrtho <- annotateAnyProbeset(ids=myProbesets$ProbeID[1:1000],
                                                          orthologue=TRUE))
##system.time(freeProbesetsMultiOrtho <- annotateAnyProbeset(ids=myProbesets$ProbeID[1:1000],
##                                                         orthologue=TRUE, multiOrth=TRUE))
system.time(freeProbesetsAllOrtho <- annotateAnyProbeset(ids=myProbesets$ProbeID,
                                                         orthologue=TRUE))
## annotateing orthologues can be still slow when there are many probesets
## system.time(ratAllOrtho <- annotateAnyProbeset(ids=largeProbesetsOrth$ProbeID[1:10000],
##                                               orthologue=TRUE, multiOrth=TRUE))
system.time(ratUniqOrtho <- annotateAnyProbeset(ids=largeProbesetsOrth$ProbeID,
                                               orthologue=TRUE, multiOrth=FALSE))
## annotateProbesets
system.time(ap <- annotateProbesets(chip="RAT230_2"))
system.time(apOrth <- annotateProbesets(chip="RAT230_2", orthologue=TRUE))
            
## Try to guess chip types
## select ProbeIDs from genome.chip_probeset_gene is slow
system.time(guessedTypes <- guessChiptype(myProbesets$ProbeID, sample=200))
system.time(guessedTypesAgain <- guessChiptype(myProbesets$ProbeID[1:150], sample=200))
system.time(bestGuessedType <- guessChiptype(myProbesets$ProbeID, maxOnly=TRUE))

system.time(myProbesets2 <- gtiChipAnnotation(chip="RAT230_2"))
system.time(guessedTypes2 <- guessChiptype(myProbesets2$ProbeID, maxOnly=FALSE, sample=250))


## annotateGeneIDs
myGeneIDs <- c(myProbesets$GeneID[1:500],
               largeProbesetsOrth$OrigGeneID[1:500])
system.time(myGeneIDAnno <- annotateGeneIDs(myGeneIDs))
system.time(myGeneIDAnnoOrth <- annotateGeneIDs(myGeneIDs, orthologue=TRUE))
##system.time(myGeneIDAnnoOrthMulti <- annotateGeneIDs(myGeneIDs[900:1000], orthologue=TRUE, multiOrth=TRUE))

## annotateGeneSymbols
myGeneSymbols <- c("AKT1", "Akt2", "Mapk14", "Cdk2", "wrong")
system.time(myGSanno <- annotateGeneSymbols(myGeneSymbols, orthologue=FALSE, organism="human"))
system.time(myGSannoAny <- annotateGeneSymbols(myGeneSymbols, orthologue=FALSE, organism="any"))
system.time(myGSannoAnyOrtho <- annotateGeneSymbols(myGeneSymbols, organism="any", orthologue=TRUE))
system.time(myGSannoAnyOrthoMulti <- annotateGeneSymbols(myGeneSymbols, organism="any", orthologue=TRUE, multiOrth=TRUE))
