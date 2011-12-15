## annotation
mgetFirst <- function(x, env) {
  sapply(mget(x, env, ifnotfound=NA),"[", 1)
}

annotateByChipname <- function(chipname, ids) {
  if(length(chipname) != 1L)
    stop("The 'chipname' must be of length one\n")
  if(!grepl("\\.db$", chipname))  {
    chip.db <- paste(chipname, ".db", sep="")
  } else {
    chip.db <- chipname
    chipname <- gsub("\\.db$", "", chipname)
  }
  installed.package.names <- installed.packages()[,"Package"]
  chip.annotation.installed <- chip.db %in% installed.package.names
  ids <- as.character(ids)
  
  annotation.df <- data.frame(probe.id=ids,
                              row.names=ids)
  if(!chip.annotation.installed & "BiocInstaller" %in% installed.package.names) {
    biocLite(chip.db)
    install.success <- chip.db %in% installed.packages()[,"Package"]
    if(!install.success) {
      return(annotation.df)
    }
  } else {
    return(annotation.df)
  }
  
  chip.db.eval <- eval(chip.db)
  require(chip.db, character.only=TRUE)
  getEnv <- function(chipname, attribute) eval(as.symbol(paste(chipname, attribute, sep="")))
  chrs <- mgetFirst(ids, getEnv(chipname, "CHR"))
  ## EntrezGeneID is forced as character to avoid the implicit converion from factor to integer
  entrezid <- as.character(mgetFirst(ids, getEnv(chipname, "ENTREZID")))
  genename <- mgetFirst(ids, getEnv(chipname, "GENENAME"))
  symbol <- mgetFirst(ids, getEnv(chipname, "SYMBOL"))
  
  annotations <- data.frame(chromosome=chrs,
                            EntrezID=I(entrezid),
                            GeneName=genename,
                            GeneSymbol=symbol)
  annotation.df <- cbind(annotation.df, annotations)
  return(annotation.df)
}
