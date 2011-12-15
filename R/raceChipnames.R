raceChipnames <- function(include.desc=FALSE) {
  con <- newcon()
  rs <- dbSendQuery(con,
                    "SELECT PCHIP_NAME, DESCR FROM bi.AFFYCHIP_NAMES")
  while(!dbHasCompleted(rs)) {
    df <- fetch(rs, n=-1)
  }
  dbClearResult(rs)
  dbDisconnect(con)
  if(include.desc) {
    res <- df
  } else {
    res <- df[,1L]
  }
  return(res)
}
raceEntrez <- function(chip) {
  con <- newcon()
  state <- paste("SELECT AFFY_ID, LL, GN, SINGLE_LL,PCHIP_NAME FROM bi.AFFYCHIP_XREF_LL where PCHIP_NAME='",
                 chip, "'", sep="")
  rs <- dbSendQuery(con, state)
  while(!dbHasCompleted(rs)) {
    ann <- fetch(rs, n=-1)
  }
  dbClearResult(rs)
  dbDisconnect(con)
  colnames(ann) <- c("ProbeID", "GeneID", "GeneSymbol", "SingleGeneID", "Chip")
  return(ann)
}

affychipNames <- function(...) {
  .Deprecated("raceChipnames",package="ribiosAnnotation")
 raceChipnames(...) 
}
