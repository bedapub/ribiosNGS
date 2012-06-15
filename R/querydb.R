RIBIOS_TMP_TBL <- "RIBIOS_ID_TMP"

querydb <- function(sqlComm, db="bia", user="biread", password="biread") {
  con <- dbConnect(ORA, user=user, password=password, db=db)
  rs <- dbSendQuery(con, sqlComm)
  while (!dbHasCompleted(rs)) {
    ann <- fetch(rs, n = -1)
  }
  dbClearResult(rs)
  dbDisconnect(con)
  ann
}


## select in: large IN queries
querydbSelectIn <- function(sqlComm, inCol, inValues,
                            db="bia", user="biread", password="biread") {
  con <- dbConnect(ORA, user=user, password=password, db=db)
  inValues <- unique(inValues)
  if(length(inValues) <= ORACLE.IN.NMAX) {
    state <- paste(sqlComm, inCol, "IN", formatIn(inValues), collapse=" ");
    querydb(state, db=db, user=user, password=password)
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


## The temporary table is created one a new Oracle() instance (e.g. a new R session) is made
## The table content is private to the session
## ON OCMMIT DELETE ROWS: the rows are cleared after dbDisconnects(db) is run
fillOneColTmpTbl <- function(con,  values) {
  if(!dbExistsTable(con, RIBIOS_TMP_TBL)) {
    state <- paste("CREATE GLOBAL TEMPORARY TABLE",
                   RIBIOS_TMP_TBL,
                   "(ID VARCHAR2(100) NOT NULl PRIMARY KEY) ON COMMIT DELETE ROWS")
    rs <- dbSendQuery(con, state)
  }
  inputDf <- data.frame(IDs=as.character(values))
  state2 <- paste("insert into", RIBIOS_TMP_TBL, "(ID) values (:1)")
  rs <- dbSendQuery(con, state2, data=inputDf)
  return(dbHasCompleted(rs))
}

## querydbTmpTbl shows principles of using temporary table. The SQL building is not finished: currently it only supports WHERE-free syntax
querydbTmpTbl <- function(sqlComm, inCol, inValues,
                          db="bia", user="biread", password="biread") {
  con <- dbConnect(ORA, user=user, password=password, db=db)
  inValues <- unique(inValues)
  fillOneColTmpTbl(con=con, values=inValues)
  hasFrom <- grepl("from", sqlComm, ignore.case=TRUE)
  hasWhere <- grepl("where", sqlComm, ignore.case=TRUE)
  if(!hasFrom) stop("Cannot find 'from' in the SQL command line\n")
  if(hasWhere) {
    state <- gsub("WHERE",
                    paste(",", RIBIOS_TMP_TBL," t WHERE t.ID=", inCol, " AND ", sep=""), sqlComm, ignore.case=TRUE)
  } else {
    sqlComm <- gsub("FROM", paste("FROM ", RIBIOS_TMP_TBL, " t,", sep=""), sqlComm, ignore.case=TRUE)
    state <- paste(sqlComm, " WHERE ", inCol, "=t.ID",sep="")
  }
  rs <- dbSendQuery(con, state)
  while (!dbHasCompleted(rs)) {
    ann <- fetch(rs, n = -1)
  }
  dbClearResult(rs)
  dbDisconnect(con)
  ann
}


