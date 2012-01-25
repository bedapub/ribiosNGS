annotateIDs <- function(ids, chip="HG-U133_PLUS_2", column="ProbeID") {
  ids <- as.character(ids)
  chipAnn <- gtiChipAnnotation(chip)
  chipAnnOrd <- matchColumn(ids, chipAnn, column)
  
  if(identical(anyDuplicated(ids), 0L) & !any(is.na(ids))) {
    rownames(chipAnnOrd) <- ids
  } else {
    rownames(chipAnnOrd) <- NULL
  }
  chipAnnOrd[,column] <- ids
  return(chipAnnOrd)
}

annotateProbesets <- function(ids, chip="HG-U133_PLUS_2") {
  annotateIDs(ids=ids, chip=chip, column="ProbeID")
}

annotateProbeIDs <- annotateProbesets

annotateGeneIDs <- function(ids, chip="HG-U133_PLUS_2", keepProbeID=TRUE) {
  chipAnnOrd <- annotateIDs(ids=ids, chip=chip, column="GeneID")
  if(!keepProbeID)
    chipAnnOrd[,"ProbeID"] <- ids
  return(chipAnnOrd)
}

annotateGeneSymbols <- function(ids, chip="HG-U133_PLUS_2", keepProbeID=TRUE) {
  chipAnnOrd <- annotateIDs(ids=ids, chip=chip, column="GeneSymbol")
  if(!keepProbeID)
    chipAnnOrd[,"ProbeID"] <- ids
  return(chipAnnOrd)
}
