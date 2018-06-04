getRemoteMartGeneAnnotationSample <- function() {
  ratMart <- useMart("ensembl", dataset="rnorvegicus_gene_ensembl")
  attributes <- c("ensembl_gene_id_version", "ensembl_gene_id","entrezgene",
                  "gene_biotype", "external_gene_name")

  rat <- biomaRt::getBM(attributes,
                        values=TRUE, mart = ratMart)
  setcolorder(rat, order(colnames(rat)))
  return(rat)
}

getRemoteMartTranscriptAnnotationSample <- function() {
  ratMart <- useMart("ensembl", dataset="rnorvegicus_gene_ensembl")
  attributes <- c("ensembl_transcript_id_version",
                  "ensembl_transcript_id", "ensembl_gene_id_version",
                  "transcript_biotype", "external_transcript_name")
  rat <- biomaRt::getBM(attributes,
                        values=TRUE, mart = ratMart)
  setcolorder(rat, order(colnames(rat)))
  rat
}

getLocalMartGeneAnnotationSample <- function() {
  conn <- dbConnect (MySQL(), user=testDB$user, password=testDB$passwd,
                     dbname="ensembl_mart_92", host=testDB$host, port=testDB$port)
  tryCatch({
    mart <- useLocalMart(conn, dataset="rnorvegicus_gene_ensembl")

    attributes <- c("ensembl_gene_id_version", "ensembl_gene_id", "entrezgene",
                    "gene_biotype", "external_gene_name")

    rat <- getLocalBM(attributes, mart = mart)
    setcolorder(rat, order(colnames(rat)))
    return(rat)
  }, finally = {
    dbDisconnect(conn)
  })
}

getLocalMartTranscriptAnnotationSample <- function() {
  conn <- dbConnect (MySQL (), user=testDB$user, password=testDB$passwd,
                     dbname="ensembl_mart_92", host=testDB$host, port=testDB$port)

  tryCatch({
    mart <- useLocalMart(conn, dataset="rnorvegicus_gene_ensembl")

    attributes <- c("ensembl_transcript_id_version", "ensembl_transcript_id", "ensembl_gene_id_version",
                    "transcript_biotype", "external_transcript_name")

    rat <- getLocalBM(attributes, mart = mart)
    setcolorder(rat, order(colnames(rat)))
    return(rat)
  }, finally = {
    dbDisconnect(conn)
  })
}

queryRemote <- function(dataset="rnorvegicus_gene_ensembl",attributes, filters="", values="", verbose=FALSE) {
  martObj <- useMart("ensembl", dataset=dataset)
  biomaRt::getBM(attributes = attributes,
                 filters = filters,
                 values = values,
                 mart = martObj,
                 verbose = verbose)
}

queryLocal <- function(dataset="rnorvegicus_gene_ensembl", attributes, filters=NULL, values=NULL, verbose=FALSE) {
  conn <- dbConnect (MySQL (), user=testDB$user, password=testDB$passwd,
                     dbname="ensembl_mart_92", host=testDB$host, port=testDB$port)

  tryCatch({
    martObj <- useLocalMart(conn, dataset=dataset)
    getLocalBM(attributes=attributes,
               filters=filters,
               values = values,
               mart=martObj,
               verbose=verbose)
  }, finally = {
    dbDisconnect(conn)
  })
}
