#' Returns table of all available BioMart datasets (see \code{\link[biomaRt]{listDatasets}}
#'
#' @param conn The \code{\linkS4class{DBIConnection}} connection object to the local Ensembl database
#' @return a \code{\link[data.table]{data.table}}
#' @export
#' @importFrom DBI dbGetQuery
#' @importClassesFrom DBI DBIConnection
listLocalDatasets <- function(conn) {
  dbGetQuery(conn, "SELECT dataset, display_name as 'description', version FROM meta_conf__dataset__main;")
}

#' Used to extract XML attribute values from an list of AttributeDescription nodes
#'
#' @param nodes list of AttributeDescription XML nodes
#' @param name the name of the attribute we want to extract
#' @return vector with XML attribute values or \code{NA} if the corresponding node doesn't have the requested XML attribute
#'
#' @importFrom XML xmlGetAttr
extractInfoFromAttributeDescriptions <- function(nodes, name) {
  unlist(lapply(nodes, function(x) {
    attr <- xmlGetAttr(x, name = name)
    if(is.null(attr)) {
      return(NA)
    }
    attr
  }))
}

#' Used to extract the meta information about BioMart attributes from XML content
#'
#' @param docRoot the root XML document containg dataset meta information
#' @return BioMart attribute meta information as \code{\link[data.table]{data.table}}
#'
#' @importFrom XML getNodeSet xmlGetAttr
#' @importFrom data.table data.table
extractListOfBioMartAttributes <- function(docRoot) {
  attributePages <- getNodeSet(docRoot, "//AttributePage")
  localAttributes <- NULL
  for (i in 1:length(attributePages)) {
    attrNodes <- getNodeSet(attributePages[[i]], "AttributeGroup/AttributeCollection/AttributeDescription")

    attributes <- data.table(
      page = rep(xmlGetAttr(attributePages[[i]], name = "internalName"), length(attrNodes)),
      primaryKey = extractInfoFromAttributeDescriptions(attrNodes, "key"),
      pointerAttribute = extractInfoFromAttributeDescriptions(attrNodes, "pointerAttribute"),
      pointerDataset = extractInfoFromAttributeDescriptions(attrNodes, "pointerDataset"),
      displayName = extractInfoFromAttributeDescriptions(attrNodes, "displayName"),
      internalName = extractInfoFromAttributeDescriptions(attrNodes, "internalName"),
      tableConstraint = extractInfoFromAttributeDescriptions(attrNodes, "tableConstraint"),
      field = extractInfoFromAttributeDescriptions(attrNodes, "field")
    )
    localAttributes <- rbind(localAttributes, attributes)
  }
  localAttributes
}

#' Creates a new mart object for the given database connection (see \code{\link[biomaRt]{useMart}}
#'
#' @param conn The \code{\linkS4class{DBIConnection}} connection object to the local Ensembl database
#' @param dataset The name of the Ensembl dataset to query (e.g. "rnorvegicus_gene_ensembl")
#' @return the mart object (regular list) with the connection object and internal attributes in it
#'
#' @export
#' @importFrom DBI dbGetQuery
#' @importFrom XML xmlParse xmlAttrs
#' @importFrom stringr str_match
useLocalMart <- function(conn, dataset) {

  xml <- dbGetQuery(conn, sprintf("SELECT xml FROM meta_conf__xml__dm AS xml_meta
                                  INNER JOIN meta_conf__dataset__main AS dataset_meta
                                  ON xml_meta.dataset_id_key = dataset_meta.dataset_id_key
                                  WHERE dataset_meta.dataset = \"%s\"", dataset))

  xmlFile <- tempfile(pattern = dataset, fileext = ".xml")
  writeLines(text = as.character(xml), con = xmlFile)

  docRoot <-xmlParse(xmlFile)

  mainTables <- xmlAttrs(getNodeSet(docRoot, "//MainTables")[[1]])

  list(dataset=dataset,
       conn = conn,
       .mainTables = data.table(primaryKey=names(mainTables),
                                table=mainTables,
                                #extract handy aliasas (i.e. "gene", "transcription, and "translation")
                                alias=str_match(mainTables, ".*_([a-z]+)__main$")[,2]),
       .attributes = extractListOfBioMartAttributes(docRoot))
}

#' Lists all available attributes for a selected dataset (see \code{\link[biomaRt]{listAttributes}}.
#'
#' @param mart the BioMart meta object needed to execute the BioMart queries on a local database server
#' @return a user-friendly and
#'
#' @export
listLocalAttributes <- function(mart) {
  data.table(
    name = mart$.attributes$internalName,
    description = mart$.attributes$displayName,
    page = mart$.attributes$page
  )
}

#' Executes a biomart query on the given mart object (see \code{\link[biomaRt]{getBM}}
#'
#' @param attributes a vector of BioMart attributes
#' @param values the values used for the BioMart filters
#' @param mart the BioMart meta object needed to execute the BioMart queries on a local database server
#' @return the mart object (regular list) with the connection object and internal attributes in it
#'
#' @export
#' @importFrom DBI dbGetQuery
#' @importFrom utils type.convert
#' @importFrom data.table data.table :=
#TODO: no filtering for now...
getLocalBM <- function(attributes, values, mart) {

  # Extract relevant attributes
  selectedAttrs <- mart$.attributes[mart$.attributes$internalName %in% attributes,]


  # Replace "main" table references with actual main tables (gene__main, transcript__main, etc.)
  selectedAttrs[tableConstraint == "main",
                tableConstraint:=lapply(
                  primaryKey,
                  function(x) mart$.mainTables[primaryKey == x, table])
                ]

  #Extract target tables
  targetTables <- unique(selectedAttrs[,.(tableConstraint,primaryKey)])
  #Generate SQL attribute lists & appropriate aliases
  attributeList <- unlist(lapply(targetTables$tableConstraint, function(targetTable) {
    targetAttrs <- selectedAttrs[selectedAttrs$tableConstraint == targetTable,]
    sprintf("%s.%s as %s", targetTable, targetAttrs$field, targetAttrs$internalName)
  }))

  #The most interesting part: We can have up to 3 different types of primary keys / foraign keys:
  # (1) The gene id for joining gene-related tables (e.g. gene_id_<internal_id>_key)
  # (2) The transcript id for joining transcript-related tables (e.g. transcript_id_<internal_id>_key)
  # (2) The translation id for joining translation-related tables (e.g. translation_id_<internal_id>_key)
  #This requires hierarchical joining of tables.

  #First: join all tables that have share a comman primary key / "main" foraing key
  whereClauses <- unlist(mapply(
    x=mart$.mainTables$table,
    y=mart$.mainTables$primaryKey,
    function(x, y) {
      tableGroup <- targetTables[primaryKey == y,]
      if(nrow(tableGroup) > 1) {
        unlist(lapply(1:(nrow(tableGroup)-1), function(i) {
          sprintf("%s.%3$s = %s.%3$s",
                  tableGroup[i,tableConstraint],
                  tableGroup[i+1,tableConstraint],
                  y)
        }))
      }
  }))

  #Second: Merge tables from different category (gen / transcript / translation)
  pkOf <- function(category) {
    mart$.mainTables[alias==category,primaryKey]
  }
  mainTableOf <- function(category) {
    mart$.mainTables[alias==category,table]
  }
  isCategoryUsed <- function(category) {
    pkOf(category) %in% targetTables$primaryKey
  }
  firstTableWithPrimaryKeyOf <- function(category) {
    targetTables[primaryKey==pkOf(category), tableConstraint]
  }

  #Join gene data with transcription data
  if(isCategoryUsed("gene") &&
     isCategoryUsed("transcript")) {
    #make sure that main table for transcription data is part of the inner joins
    if(!(mainTableOf("transcript") %in% targetTables$tableConstraint)) {
      #join transcription main table with any table with an transcription id
      whereClauses <- c(whereClauses,
                        sprintf("%s.%3$s = %s.%3$s",
                                mainTableOf("transcript"),
                                firstTableWithPrimaryKeyOf("transcript"),
                                pkOf("transcription")))
    }
    #join transcription main table with any table with a gene id
    whereClauses <- c(whereClauses,
                      sprintf("%s.%3$s = %s.%3$s",
                              firstTableWithPrimaryKeyOf("gene"),
                              mainTableOf("transcript"),
                              pkOf("gene")))
  }

  #Join transcript data with translation data
  if(isCategoryUsed("transcript") &&
     isCategoryUsed("translation")) {
    #Make sure that main table for translation data is part of the inner joins
    if(!(mainTableOf("translation") %in% targetTables$tableConstraint)) {
      #Join transcription main table with any table with an transcription id
      whereClauses <- c(whereClauses,
                        sprintf("%s.%3$s = %s.%3$s",
                                mainTableOf("translation"),
                                firstTableWithPrimaryKeyOf("translation"),
                                pkOf("transation")))
    }
    #join translation main table with any table with a transcription id
    whereClauses <- c(whereClauses,
                      sprintf("%s.%3$s = %s.%3$s",
                              firstTableWithPrimaryKeyOf("transcript"),
                              mainTableOf("translation"),
                              pkOf("transcript")))
  }

  #Join gene data with translation data
  if(isCategoryUsed("gene") &&
     !isCategoryUsed("transcript") &&
     isCategoryUsed("translation")) {
    #make sure that main table for translation data is part of the inner joins
    if(!(mainTableOf("translation") %in% targetTables$tableConstraint)) {
      #join translation main table with any table with an translation id
      whereClauses <- c(whereClauses,
                        sprintf("%s.%3$s = %s.%3$s",
                                mainTableOf("translation"),
                                firstTableWithPrimaryKeyOf("translation"),
                                pkOf("translation")))
    }
    #join translation main table with any table with a gene id
    whereClauses <- c(whereClauses,
                      sprintf("%s.%3$s = %s.%3$s",
                              firstTableWithPrimaryKeyOf("gene"),
                              mainTableOf("translation"),
                              pkOf("gene")))
  }

  query <- sprintf("SELECT %s FROM %s",
                   paste(attributeList, collapse = ","),
                   paste(targetTables$tableConstraint, collapse = ","))
  if(length(targetTables) > 1) {
    query <- sprintf("%s WHERE %s", query, paste(whereClauses, collapse = " AND "))
  }
  print(query)
  result <- dbGetQuery(mart$conn, query)
  #Remove factor columns and replace them by numeric values whenever possible
  for(col in colnames(result)) {
    result[[col]] <- type.convert(as.character(result[[col]]),as.is=TRUE)
  }
  result
}

#source("tests/testthat/helper.quersFun.R")

#geneLocal <- getLocalMartGeneAnnotationSample()
#geneRemote <- getRemoteMartGeneAnnotationSample()
