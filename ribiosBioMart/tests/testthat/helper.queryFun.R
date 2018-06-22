library (biomaRt)
library (data.table)
library (RMySQL)

queryRemote <- function(dataset, attributes, filters="", values="", verbose=FALSE) {
  martObj <- useMart("ensembl", dataset=dataset)
  result <- biomaRt::getBM(attributes = attributes,
                           filters = filters,
                           values = values,
                           mart = martObj,
                           verbose = verbose)
  setcolorder(result, order(colnames(result)))
  #Ensembl BioMart reports values with a blank istead of NA, don't know if this is allways the case
  result[result == ""] <- NA
  result
}

queryLocal <- function(dataset, attributes, filters=NULL, values=NULL, verbose=FALSE) {
  conn <- dbConnect (MySQL (), user=testDB$user, password=testDB$passwd,
                     dbname="ensembl_mart_92", host=testDB$host, port=testDB$port)
  tryCatch({
    martObj <- useLocalMart(conn, dataset=dataset)
    result <- getLocalBM(attributes=attributes,
                         filters=filters,
                         values = values,
                         mart=martObj,
                         verbose=verbose)
    setcolorder(result, order(colnames(result)))
    result
  }, finally = {
    dbDisconnect(conn)
  })
}
