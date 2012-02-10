mappedProbesets <- function(geneids,
                            chip="HG-U133_PLUS_2",
                            isSingleGeneID=FALSE,
                            unlist=FALSE) {
  ids <- as.character(geneids)
  chipAnno <- gtiChipAnnotation(chip, inCol="GeneID", inValues=geneids)
  if(isSingleGeneID)
    chipAnno <- subset(chipAnno, isSingleGeneID)
  mapId <- with(chipAnno, split(ProbeID, GeneID))
  if(unlist)
    mapId <- unlist(mapId, use.names=FALSE)
  return(mapId)
}
