## query current GeneID/GeneSymbol from BIOS Oracle
raceChipGenes <- function(ids, arrayType="HG-U133_PLUS_2") {
  if(!require(ROracle))
    stop("ROracle needed")

  ids <- as.character(ids)
  
  ora <- dbDriver("Oracle")
  con <- dbConnect(ora, "genome/genome@bin")
  comm <- sprintf("select g.gene_symbol, p.probeset_id from chip_probeset_gene p, gti_genes g \
                   where g.ro_gene_id = p.ro_gene_id and p.array_type = '%s'",
                  arrayType)
  rs <- dbSendQuery(con, comm )
  while(!dbHasCompleted(rs)) {
    df <- fetch(rs, n=-1)
    ## process(df)
  }
  dbClearResult(rs)

  updated.geneSymbols <- df$GENE_SYMBOL[matchColumnIndex(ids, df, "PROBESET_ID")]
  rm(df)
  return(updated.geneSymbols)
}

biosCurrentGeneSymbol <- function(...) {
  .Deprecated("raceChipGenes",
              package="ribiosAnnotation")
  raceChipGenes(...)
}
