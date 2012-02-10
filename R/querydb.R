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


## select in: large IN queries
querydbSelectIn <- function(sqlComm, inCol, inValues, db=c("bia", "bin")) {
  db <- match.arg(db)
  if(db=="bia") {
    con <- newconBIA()
  } else if (db=="bin") {
    con <- newconBIN()
  }
  inValues <- unique(inValues)
  if(length(inValues) <= ORACLE.IN.NMAX) {
    state <- paste(sqlComm, inCol, "IN", formatIn(inValues), collapse=" ");
    querydb(state, db=db)
  } else {
    nob <- ceiling(length(inValues)/ORACLE.IN.NMAX)
    res <- vector("list", nob)
    for(i in 1:nob) {
      ind <- seq((i-1)*ORACLE.IN.NMAX+1L,
               i*ORACLE.IN.NMAX)
      state <- paste(sqlComm, inCol, "IN", formatIn(inValues[ind]))
      rs <- dbSendQuery(con, state)
      while (!dbHasCompleted(rs)) {
        ann <- fetch(rs, n = -1)
      }
      dbClearResult(rs)
      res[[i]] <- ann
    }
    dbDisconnect(con)
    do.call(rbind, res)
  }
}
