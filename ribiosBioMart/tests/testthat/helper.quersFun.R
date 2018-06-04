getRemoteMartGeneAnnotationSample <- function() {
  ratMart <- useMart("ensembl", dataset="rnorvegicus_gene_ensembl")
  attributes <- c("ensembl_gene_id_version", "ensembl_gene_id","entrezgene",
                  "gene_biotype", "external_gene_name")

  rat <- biomaRt::getBM(attributes,
                        values=TRUE, mart = ratMart)
  ratAnno <- data.frame(TaxID=10116L,
                        EnsembleGeneID=rat$ensembl_gene_id_version,
                        StableEnsembleGeneID=rat$ensembl_gene_id,
                        GeneID=rat$entrezgene,
                        GeneType=rat$gene_biotype,
                        GeneSymbol=rat$external_gene_name)
  ratAnno
}

getRemoteMartTranscriptAnnotationSample <- function() {
  ratMart <- useMart("ensembl", dataset="rnorvegicus_gene_ensembl")
  attributes <- c("ensembl_transcript_id_version",
                  "ensembl_transcript_id", "ensembl_gene_id_version",
                  "transcript_biotype", "external_transcript_name")
  rat <- biomaRt::getBM(attributes,
                        values=TRUE, mart = ratMart)
  ratAnno <- data.frame(TaxID=10116L,
                        EnsembleTranscriptID=rat$ensembl_transcript_id_version,
                        StableEnsembleTranscriptID=rat$ensembl_transcript_id,
                        EnsembleGeneID=rat$ensembl_gene_id,
                        TranscriptType=rat$transcript_biotype,
                        TranscriptName=rat$external_transcript_name)
  ratAnno
}

getLocalMartGeneAnnotationSample <- function() {
  conn <- dbConnect (MySQL(), user=testDB$user, password=testDB$passwd,
                     dbname="ensembl_mart_92", host=testDB$host, port=testDB$port)
  tryCatch({
    mart <- useLocalMart(conn, dataset="rnorvegicus_gene_ensembl")

    attributes <- c("ensembl_gene_id_version", "ensembl_gene_id", "entrezgene",
                    "gene_biotype", "external_gene_name")

    rat <- getLocalBM(attributes, mart = mart)

    ratAnno <- data.frame(TaxID=10116L,
                          EnsembleGeneID=rat$ensembl_gene_id_version,
                          StableEnsembleGeneID=rat$ensembl_gene_id,
                          GeneID=rat$entrezgene,
                          GeneType=rat$gene_biotype,
                          GeneSymbol=rat$external_gene_name)
    return(ratAnno)
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

    ratAnno <- data.frame(TaxID=10116L,
                          EnsembleTranscriptID=rat$ensembl_transcript_id_version,
                          StableEnsembleTranscriptID=rat$ensembl_transcript_id,
                          EnsembleGeneID=rat$ensembl_gene_id,
                          TranscriptType=rat$transcript_biotype,
                          TranscriptName=rat$external_transcript_name)
    return(ratAnno)
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

queryLocal <- function(dataset="rnorvegicus_gene_ensembl", attributes, filters="", values="", verbose=FALSE) {
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
