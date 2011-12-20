## query current GeneID/GeneSymbol from BIOS Oracle
gtiChipAnnotation <- function(chip) {
  if(missing(chip)) {
    stop("'chip' cannot be be missing. Use 'raceChipnames()' to see supported chip names")
  }
  con <- newcon()
  state <- paste("SELECT AFFY_ID, LL, GN, SINGLE_LL,PCHIP_NAME FROM bi.AFFYCHIP_XREF_LL where PCHIP_NAME='",
                 chip, "'", sep="")
  rs <- dbSendQuery(con, state)
  while(!dbHasCompleted(rs)) {
    ann <- fetch(rs, n=-1)
  }
  dbClearResult(rs)
  dbDisconnect(con)
  colnames(ann) <- c("ProbeID", "GeneID", "GeneSymbol", "isSingleGeneID", "Chip")
  ann[,"isSingleGeneID"] <- as.logical(ann[,"isSingleGeneID"])
  rownames(ann) <- NULL
  return(ann)
}



biosCurrentGeneSymbol <- function(...) {
  .Deprecated("gtiChipAnnotation",
              package="ribiosAnnotation")
  gtiChipAnnotation(...)
}

raceChipAnnotation <- function(...) {
  .Deprecated("gtiChipAnnotation",
              package="ribiosAnnotation")
  gtiChipAnnotation(...)
}
