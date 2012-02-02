## query current GeneID/GeneSymbol from BIOS Oracle
gtiChipAnnotation <- function(chip) {
  if(missing(chip)) {
    stop("'chip' cannot be be missing. Use 'gtiChipnames()' to see supported chip names")
  }
  con <- newconBIA()
  state <- paste("SELECT a.AFFY_ID, a.LL, e.OFFICIAL_SYMBOL, e.OFFICIAL_NAME, a.SINGLE_LL,a.PCHIP_NAME ",
                 "FROM bi.AFFYCHIP_XREF_LL a, bi.EG_GENE_INFO e ",
                 "where a.LL = e.GENEID ",
                 "AND PCHIP_NAME='",chip, "'", sep="")
  rs <- dbSendQuery(con, state)
  while(!dbHasCompleted(rs)) {
    ann <- fetch(rs, n=-1)
  }
  dbClearResult(rs)
  dbDisconnect(con)
  colnames(ann) <- c("ProbeID", "GeneID", "GeneSymbol", "GeneName", "isSingleGeneID", "Chip")
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
