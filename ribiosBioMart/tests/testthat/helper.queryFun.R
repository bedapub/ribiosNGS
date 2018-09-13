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
  conn <- EnsemblDBCredentials(host = testDB$host,
                               port = testDB$port,
                               user = testDB$user,
                               passwd = testDB$passwd,
                               ensembl_version = 93)

  martObj <- useLocalMart(conn, dataset = dataset)
  result <- getLocalBM(attributes = attributes,
                       filters = filters,
                       values = values,
                       mart = martObj,
                       verbose = verbose)
  setcolorder(result, order(colnames(result)))
  result
}
