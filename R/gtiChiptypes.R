gtiChiptypes <- function(include.desc=FALSE) {
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

gtiChipnames <- function(...) {gtiChiptypes(...)}
affychipNames <- function(...) {
  .Deprecated("gtiChiptypes",package="ribiosAnnotation")
  gtiChipnames(...) 
}
raceChiptypes <- function(...) {
  .Deprecated("gtiChiptypes",package="ribiosAnnotation")
  gtiChiptypes(...) 
}
raceChipnames <- function(...) {
  .Deprecated("gtiChipnames",package="ribiosAnnotation")
  gtiChipnames(...) 
}
