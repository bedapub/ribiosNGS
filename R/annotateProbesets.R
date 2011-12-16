annotateProbesets <- function(ids, chip="HG-U133_PLUS_2") {
  ids <- as.character(ids)

  chipAnn <- raceChipAnnotation(chip)
  chipAnnOrd <- matchColumn(ids, chipAnn, "ProbeID")
  if(identical(anyDuplicated(ids), 0L)) {
    rownames(chipAnnOrd) <- ids
  } else {
    rownames(chipAnnOrd) <- NULL
  }
  return(chipAnnOrd)
}
