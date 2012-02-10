chrLen <- function(organism=c("human", "mouse", "rat")) {
  organism <- match.arg(organism)
  if(organism=="human") {
    db <- "HUMANN"
  } else if(organism=="mouse") {
    db <- "MOUSEN"
  } else if (organism=="rat") {
    db <- "RATN"
  }
  
  con <- newconBIN()
  state <- paste("SELECT SEQ, DESCR, SEQLEN, RANK ",
                 "FROM genome_sequence ",
                 "WHERE DB='", db, "'", sep="")
  
  rs <- dbSendQuery(con, state)
  while (!dbHasCompleted(rs)) {
    ann <- fetch(rs, n = -1)
  }
  dbClearResult(rs)
  dbDisconnect(con)
  colnames(ann) <- c("Chromosome", "Description", "Length", "Rank")
  ann$Chromosome <- gsub("^CHR","", ann$Chromosome)
  rownames(ann) <- NULL
  ann
}
