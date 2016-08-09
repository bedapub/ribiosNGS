RIBIOS_TMP_TBL <- "RIBIOS_ID_TMP"
RIBIOS_JDBC_TMP_TBL <- "RIBIOS_JDBC_ID_TMP"

## format IN syntax
#' @export formatIn
formatIn <- function(x) paste("(",paste("'", x, "'", sep="", collapse=","),")", sep="")
tmpTbl <- function(forceJDBC=FALSE) ifelse(hasOracle() & !forceJDBC, RIBIOS_TMP_TBL, RIBIOS_JDBC_TMP_TBL)

#' @export querydb
querydb <- function(sqlComm, db="bia", user="biread", password="biread", forceJDBC=FALSE) {
  isORA <- hasOracle() & !forceJDBC
  con <- ribiosCon(db=db, user=user, password=password, forceJDBC=forceJDBC)
  rs <- dbSendQuery(con, sqlComm)
  if(isORA) {
    while (!dbHasCompleted(rs))
      ann <- fetch(rs, n = -1)
  } else {
    ann <- fetch(rs, n=-1)
  }
  dbClearResult(rs)
  dbDisconnect(con)
  ann
}


## select in: large IN queries
#' @export querydbSelectIn
querydbSelectIn <- function(sqlComm, inCol, inValues,
                            db="bia", user="biread", password="biread",
                            forceJDBC=FALSE) {
  isORA <- hasOracle() & !forceJDBC
  inValues <- unique(as.character(inValues))
  if (length(inValues) <= ORACLE.IN.NMAX) {
    state <- paste(sqlComm, inCol, "IN", formatIn(inValues), 
                   collapse = " ")
    querydb(state, db = db, user = user, password = password, forceJDBC=forceJDBC)
  } else {
    con <- ribiosCon(db = db, user = user, password = password, forceJDBC=forceJDBC)
    nob <- ceiling(length(inValues)/ORACLE.IN.NMAX)
    res <- vector("list", nob)
    for (i in 1:nob) {
      ind <- seq((i - 1) * ORACLE.IN.NMAX + 1L, i * ORACLE.IN.NMAX)
      state <- paste(sqlComm, inCol, "IN", formatIn(inValues[ind]))
      rs <- dbSendQuery(con, state)
      if(isORA) {
        while (!dbHasCompleted(rs))
          ann <- fetch(rs, n = -1)
      } else {
        ann <- fetch(rs, n=-1)
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
## ON OCMMIT DELETE ROWS: the rows are cleared after dbCommit() is run
fillOneColTmpTbl <- function(con,  values) {
  values <- as.character(values)
  values[is.na(values)] <- "NA"
  values[values==""] <- "NA"
  isORA <- inherits(con, "OraConnection")
  if(isORA) {
    if (!dbExistsTable(con, RIBIOS_TMP_TBL)) {
      state <- paste("CREATE GLOBAL TEMPORARY TABLE", RIBIOS_TMP_TBL, 
                     "(ID VARCHAR2(100) NOT NULl PRIMARY KEY) ON COMMIT DELETE ROWS")
      rs <- dbSendQuery(con, state)
    }
    inputDf <- data.frame(ID = values)
    state2 <- paste("insert into", RIBIOS_TMP_TBL, "(ID) values (:1)")
    rs <- dbSendQuery(con, state2, data = inputDf)
    return(dbHasCompleted(rs))
  } else {
    if(!dbExistsTable(con, RIBIOS_JDBC_TMP_TBL)) {
      state <- paste("CREATE GLOBAL TEMPORARY TABLE", RIBIOS_JDBC_TMP_TBL, 
                     "(ID VARCHAR2(100) NOT NULl PRIMARY KEY) ON COMMIT PRESERVE ROWS")
      rs <- RJDBC::dbSendUpdate(con, state)
    }
    state2 <- paste("insert into",RIBIOS_JDBC_TMP_TBL, " (ID) values (?)")
    ## TODO: SLOW: batch insert is desired
    for(i in seq(along=values))
      rs <- RJDBC::dbSendUpdate(con, state2, values[i])
    return(TRUE)
  }
}

## querydbTmpTbl shows principles of using temporary table. The SQL building is not finished: currently it only supports WHERE-free syntax
#' @export querydbTmpTbl
querydbTmpTbl <- function(sqlComm, inCol, inValues,
                          db="bia", user="biread", password="biread", forceJDBC=FALSE) {
  isORA <- hasOracle() & !forceJDBC
  con <- ribiosCon(db=db, user=user, password=password, forceJDBC=forceJDBC)
  inValues <- setdiff(unique(as.character(inValues)), "")
  
  TMP_TBL <- tmpTbl(forceJDBC=forceJDBC)
  fillOneColTmpTbl(con = con, values = inValues)
  hasFrom <- grepl("from", sqlComm, ignore.case = TRUE)
  hasWhere <- grepl("where", sqlComm, ignore.case = TRUE)
  if (!hasFrom) 
    stop("Cannot find 'from' in the SQL command line\n")
  if (hasWhere) {
    state <- gsub("WHERE", paste(",", TMP_TBL, " t WHERE t.ID=", 
                                 inCol, " AND ", sep = ""), sqlComm, ignore.case = TRUE)
  } else {
    sqlComm <- gsub("FROM", paste("FROM ", TMP_TBL, 
                                  " t,", sep = ""), sqlComm, ignore.case = TRUE)
    state <- paste(sqlComm, " WHERE ", inCol, "=t.ID", sep = "")
  }
  rs <- dbSendQuery(con, state)
  if(isORA) {
    while (!dbHasCompleted(rs))
      ann <- fetch(rs, n = -1)
  } else {
    ann <- fetch(rs, n=-1)
  }
  dbClearResult(rs)
  dbDisconnect(con)
  ann
}


