## query current GeneID/GeneSymbol from BIOS Oracle
gtiChipAnnotation <- function(chip) {
  if(missing(chip)) {
    stop("'chip' cannot be be missing. Use 'gtiChipnames()' to see supported chip names")
  }
  con <- newcon()
  state <- paste("SELECT a.AFFY_ID, a.LL, a.GN, e.OFFICIAL_NAME, a.SINGLE_LL,a.PCHIP_NAME ",
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
gtiTaxAnnotation <- function(taxid) {
  if(missing(taxid))
    stop("'taxid' cannot be missing.")
  con <- newcon()
  state <- paste("SELECT g.GENEID, g.OFFICIAL_SYMBOL, g.SYNONYMS, g.DBXREFS, ",
                 "g.CHROMOSOME, g.MAP_LOCATION, m.SEQ, m.CLEFT, m.CRIGHT, m.REVCOMP, g.DESCRIPTION, g.GENE_TYPE, ",
                 "g.OFFICIAL_NAME ",
                 "FROM bi.EG_GENE_INFO g, genome.gti_gene_map_a@genome m ",
                 "where TO_CHAR(g.GENEID)=m.RO_GENE_ID AND TAX_ID = '",taxid, "'", sep="")
  rs <- dbSendQuery(con, state)
  while(!dbHasCompleted(rs)) {
    ann <- fetch(rs, n=-1)
  }
  dbClearResult(rs)
  dbDisconnect(con)
  colnames(ann) <- c("GeneID", "GeneSymbol", "Synonyms", "xrefs",
                     "Chromosome", "MapLocation",
                     "MappedChromosome", "LeftCoord", "RightCoord", "RevComp",
                     "Description", "GeneType",
                     "GeneName")
  ## post-precessing
  ann$MappedChromosome <- gsub("^CHR", "", as.character(ann$MappedChromosome))
  ann$RevComp <- ifelse(ann$RevComp>0, TRUE, FALSE)
  uniqMapped <- with(ann, tapply(paste(MappedChromosome, LeftCoord, RightCoord),
                                 factor(GeneID), length)) == 1L
  isUniq <- uniqMapped[match(ann$GeneID, names(uniqMapped))]
  ann$UniqueMapped <- isUniq
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
