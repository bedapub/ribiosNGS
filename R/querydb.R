querydb <- function(sqlComm, db=c("bia", "bin")) {
  db <- match.arg(db)
  if(db=="bia") {
    con <- newconBIA()
  } else if (db=="bin") {
    con <- newconBIN()
  }
  
  rs <- dbSendQuery(con, sqlComm)
  while (!dbHasCompleted(rs)) {
    ann <- fetch(rs, n = -1)
  }
  dbClearResult(rs)
  dbDisconnect(con)
  ann
}
