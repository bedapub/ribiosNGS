raceChiptypes <- function(include.desc=FALSE) {
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

raceChipnames <- function(...) {raceChiptypes(...)}
affychipNames <- function(...) {
  .Deprecated("raceChiptypes",package="ribiosAnnotation")
 raceChipnames(...) 
}
