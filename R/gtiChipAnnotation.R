## format IN syntax
formatIn <- function(x)
  paste("(",paste("'", x, "'", sep="", collapse=","),")", sep="")

## query current GeneID/GeneSymbol from BIOS Oracle
gtiChipAnnotation <- function(chip,
                              inCol,
                              inValues) {
  if(missing(chip))
    stop("'chip' cannot be be missing. Use 'gtiChipnames()' to see supported chip names")
  
  state <- paste("SELECT a.PROBESET_ID, e.RO_GENE_ID, e.GENE_SYMBOL, e.DESCRIPTION, 'TRUE' AS isSingleGeneID, a.ARRAY_TYPE ",
                 "FROM genome.chip_probeset_gene a, genome.GTI_GENES e ",
                 "where a.RO_GENE_ID(+) = e.RO_GENE_ID ",
                 "AND ARRAY_TYPE='",chip, "'", sep="")
  
  hasInCol <- !missing(inCol)
  hasInVal <- !missing(inValues)
  filterInSQL <- TRUE
  
  if(hasInCol && hasInVal) {
    inValues <- unique(inValues)
    filterInSQL <- length(inValues) <= ORACLE.IN.NMAX
    if(filterInSQL) {
      inCol <- match.arg(inCol,
                         c("ProbeID", "GeneID", "GeneSymbol"),
                         several.ok=FALSE)
      inColTrans <- c("ProbeID"="a.PROBESET_ID", "GeneID"="a.RO_GENE_ID", "GeneSymbol"="e.GENE_SYMBOL")[inCol]
      state <- paste(state, " AND ", inColTrans, " IN ",
                     formatIn(inValues));
    }
  } else if (!(!hasInCol && !hasInVal)) {
    stop("'inCol' and 'inVal' must be both specified or left as missing simultaneously\n")
  }
  
  ann <- querydb(state, db="bin")
  colnames(ann) <- c("ProbeID", "GeneID", "GeneSymbol", "GeneName", "isSingleGeneID", "Chip")
  asg <- as.logical(ann[,"isSingleGeneID"])
  ann[,"isSingleGeneID"] <- asg
  ann[is.na(asg),"isSingleGeneID"] <- FALSE
  
  if(hasInCol && !filterInSQL) {
    filter <- ann[, inCol] %in% inValues
    ann <- ann[filter,]
  }
  
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
