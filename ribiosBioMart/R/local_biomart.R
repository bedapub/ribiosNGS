#' This class is used to store credentials used to connect a MySQL / MariaDB fork of the Ensembl database
#' @param host The host name of the SQL database containing a copy of the Ensembl BioMart data
#' @param port The port to the SQL database
#' @param user The user name used to authenticate to the SQL database
#' @param passwd The database password used to connect to the SQL database
#' @param ensembl_version The version of the Ensembl data inside the target SQL database. Will be used as table suffix (e.g. for "ensembl_mart_92")
#' @exportClass EnsemblDBCredentials
EnsemblDBCredentials <- setClass("EnsemblDBCredentials",
                                 representation(host = "character",
                                                port = "numeric",
                                                user = "character",
                                                passwd = "character",
                                                ensembl_version = "numeric"))

#' Creates a new DBI connection based on the given credential object
#' @export
setGeneric("createDBIConnection", valueClass = "DBIConnection", function(object) {
  standardGeneric("createDBIConnection")
})


#' Creates a new DBI connection based on the given credential object
#' @export
setMethod("createDBIConnection", signature("EnsemblDBCredentials"), function(object) {
  dbConnect (MySQL(),
             user=object@user,
             password=object@passwd,
             host=object@host,
             port=object@port,
             dbname=sprintf("ensembl_mart_%d", object@ensembl_version))
})


#' Returns table of all available BioMart datasets (see \code{\link[biomaRt]{listDatasets}}
#'
#' @param conn The \code{\linkS4class{DBIConnection}} connection object to the local Ensembl database or a instance of \code{\linkS4class{EnsemblDBCredentials}}
#' @return a \code{\link[data.table]{data.table}}
#' @export
#' @importFrom DBI dbGetQuery
#' @importClassesFrom DBI DBIConnection
listLocalDatasets <- function(conn) {
  queryStr <- "SELECT dataset, display_name as 'description', version FROM meta_conf__dataset__main;"
  if(is(conn, "DBIConnection")) {
    return(dbGetQuery(conn, queryStr))
  }

  conn <- createDBIConnection(conn)

  result <- dbGetQuery(conn, "SELECT dataset, display_name as 'description', version FROM meta_conf__dataset__main;")
  return(result)
}

#' Returns options of the given filters as list
#'
#' @param filter the name of the filters as character vector
#' @param mart the BioMart meta object needed to execute the BioMart queries on a local database server
#' @return a list of filter options available for each givn filter
#'
#' @export
#' @importFrom stringr str_split
localFilterOptions <- function(filter, mart) {
  if (missing(filter))
    stop("No filter given. Please specify the filter for which you want to retrieve the possible values.")
  if (class(filter) != "character")
    stop("Filter argument should be of class character")

  checkFilters = filter %in% mart$.filters$internalName
  if (!all(checkFilters)) {
    stop(sprintf("Unknown filters: %s", paste(filter[!checkFilters],collapse = ",")))
  }
  filtered <- mart$.filters[internalName %in% filter]
  mapply(x=filtered$internalName,y=filtered$option, function(x, y) {
    str_split(y, ",", simplify = TRUE)
  })
}

#' Returns the types of the given filters as vector
#'
#' @param filter the name of the filters as character vector
#' @param mart the BioMart meta object needed to execute the BioMart queries on a local database server
#' @return a vector of filter types
#'
#' @export
localFilterType <- function(filter, mart) {
  if (missing(filter))
    stop("No filter given. Please specify the filter for which you want to retrieve the possible values.")
  if (class(filter) != "character")
    stop("Filter argument should be of class character")

  checkFilters = filter %in% mart$.filters$internalName
  if (!all(checkFilters)) {
    stop(sprintf("Unknown filters: %s", paste(filter[!checkFilters],collapse = ",")))
  }
  mart$.filters[internalName %in% filter]$type
}

#' Creates a new mart object for the given database connection (see \code{\link[biomaRt]{useMart}}
#'
#' @param conn The \code{\linkS4class{DBIConnection}} connection object to the local Ensembl database or an instance of \code{\linkS4class{EnsemblDBCredentials}}
#' @param dataset The name of the Ensembl dataset to query (e.g. "rnorvegicus_gene_ensembl")
#' @return the mart object (regular list) with the connection object and internal attributes in it
#'
#' @export
#' @importFrom RMySQL MySQL
#' @importFrom DBI dbConnect
#' @importFrom DBI dbGetQuery
#' @importFrom DBI dbDisconnect
#' @importFrom XML xmlParse xmlAttrs getNodeSet xmlGetAttr
#' @importFrom data.table data.table
#' @importFrom stringr str_match
useLocalMart <- function(conn, dataset) {

  close_connection <- FALSE
  used_conn <- conn

  if (is(conn, "EnsemblDBCredentials")) {
    used_conn <- createDBIConnection(conn)
    close_connection <- TRUE
  }

  xml <- dbGetQuery(used_conn, sprintf("SELECT xml FROM meta_conf__xml__dm AS xml_meta
                                  INNER JOIN meta_conf__dataset__main AS dataset_meta
                                  ON xml_meta.dataset_id_key = dataset_meta.dataset_id_key
                                  WHERE dataset_meta.dataset = \"%s\"", dataset))

  if(close_connection)
    dbDisconnect(used_conn)

  xmlFile <- tempfile(pattern = dataset, fileext = ".xml")
  writeLines(text = as.character(xml), con = xmlFile)

  docRoot <-xmlParse(xmlFile)

  mainTables <- xmlAttrs(getNodeSet(docRoot, "//MainTables")[[1]])
  mainTables <- data.table(primaryKey=names(mainTables),
                           table=mainTables,
                           #extract handy aliasas (i.e. "gene", "transcription, and "translation")
                           alias=str_match(mainTables, ".*_([a-z]+)__main$")[,2])

  list(dataset=dataset,
       .conn = conn,
       .mainTables = mainTables,
       .attributes = extractListOfBioMartAttributes(dataset, docRoot, mainTables),
       .filters = extractListOfBioMartFilters(dataset, docRoot, mainTables))
}

#' Lists all available filters for a selected dataset (see \code{\link[biomaRt]{listFilters}}.
#'
#' @param mart the BioMart meta object needed to execute the BioMart queries on a local database server
#' @return a list of all available filters as \code{\link[data.table]{data.table}}
#'
#' @export
listLocalFilters <- function(mart) {
  data.table(
    name = mart$.filters$internalName,
    description = mart$.filters$displayName,
    type = mart$.filters$type,
    options = mart$.filters$options
  )
}

#' Lists all available attributes for a selected dataset (see \code{\link[biomaRt]{listAttributes}}.
#'
#' @param mart the BioMart meta object needed to execute the BioMart queries on a local database server
#' @return a list of all available attributes as \code{\link[data.table]{data.table}}
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
#' @param filters a vector of BioMart filters
#' @param values the values used for the BioMart filters
#' @param mart the BioMart meta object needed to execute the BioMart queries on a local database server
#' @param verbose Prints out the composed SQL query to the command line
#' @param uniqueRows use \code{SELECT DISTINCT} to ensure unique result rows. Default is \code{TRUE}
#' @return the mart object (regular list) with the connection object and internal attributes in it
#'
#' @export
#' @importFrom DBI dbGetQuery
#' @importFrom stringr str_split
#' @importFrom utils type.convert
#' @importFrom data.table data.table :=
getLocalBM <- function(attributes, filters=NULL, values=NULL, mart, verbose=FALSE, uniqueRows=TRUE) {

  # General parameter checking ----

  if (is.null(attributes))
    stop("Argument 'attributes' must be specified.")
  if (is.list(filters) && !is.null(values))
    warning("Argument 'values' should not be used when argument 'filters' is a list and will be ignored.")
  if (is.list(filters) && is.null(names(filters)))
    stop("Argument 'filters' must be a named list when sent as a list.")
  if (!is.null(filters) && !is.list(filters) && is.null(values))
    stop("Argument 'values' must be specified.")
  if (is.list(filters)) {
    values = filters
    filters = names(filters)
  }
  if(!is.null(values) && !is.list(values)) {
    values <- list(values)
  }
  if (!is.null(filters) && !is.null(values) && length(filters) != length(values))
    stop("Lengths of 'filters' and 'values' do not match")
  if (class(verbose) != "logical") {
    stop("Argument 'verbose' must be a logical value, so either TRUE or FALSE")
  }
  if (class(uniqueRows) != "logical") {
    stop("Argument 'uniqueRows' must be a logical value, so either TRUE or FALSE")
  }

  #Input validation: do specified attributes / filters exist?
  attrCheck <- attributes %in% mart$.attributes$internalName
  if(!all(attrCheck)) {
    stop(sprintf("Unknown attributes: %s", paste(attributes[!attrCheck],collapse = ",")))
  }
  filterCheck <- filters %in% mart$.filters$internalName
  if(!all(filterCheck)) {
    stop(sprintf("Unknown filters: %s", paste(filters[!filterCheck],collapse = ",")))
  }

  # Filter & attribute extraction ----

  selectedAttrs <- mart$.attributes[mart$.attributes$internalName %in% attributes,]
  selectedFilters <- mart$.filters[internalName %in% filters,]

  #Check if user request an attribute / filter we do not support
  attrSupport <- is.na(selectedAttrs$pointerDataset)
  if(!all(attrSupport)) {
    stop(sprintf("Unsupported attributes: %s",
                 paste(selectedAttrs$internalName[!attrSupport],collapse = ",")))
  }
  filterSupport <- is.na(selectedFilters$pointerDataset)
  if(!all(filterSupport)) {
    stop(sprintf("Unsupported filters: %s",
                 paste(selectedFilters$internalName[!filterSupport],collapse = ",")))
  }

  # Extract target tables & list of SQL attributes ----

  targetTables <- unique(rbind(
    selectedAttrs[,.(tableConstraint,primaryKey)],
    selectedFilters[,.(tableConstraint,primaryKey)]))

  #Generate SQL attribute lists & appropriate aliases
  attributeList <- unlist(lapply(targetTables$tableConstraint, function(targetTable) {
    targetAttrs <- selectedAttrs[selectedAttrs$tableConstraint == targetTable,]
    sprintf("`%s`.`%s` as `%s`", targetTable, targetAttrs$field, targetAttrs$internalName)
  }))

  #List of tables in FROM clause (can change depending on requested attributes)
  fromList <- targetTables$tableConstraint

  # Translating filters to SQL conditions (also check filter validity) ----

  # There exist the following filter "=","=,in",">=","<=", "only,excluded"
  filterList <- unlist(mapply(x=filters,y=values, function(x, y) {
    filter <- selectedFilters[internalName==x, ]
    #Handle boolean filters

    if(filter$operation == "only,excluded") {
      if(!is.logical(y) || length(y) > 1) {
        stop(sprintf("Value for boolean filter '%s' bust be a logic vector with exactly 1 element: '%s'",
                     x, paste(y,collapse = ",")))
      }
      if(y) {
        return(sprintf("`%s`.`%s` IS NOT NULL",
                       filter$tableConstraint,
                       filter$field,
                       y))
      }
      return(sprintf("`%s`.`%s` IS NULL",
                     filter$tableConstraint,
                     filter$field,
                     y))
    }
    #For anything else: check if the ther is an option list and ensure
    #The specified values are taken from that list
    allowedValues <- str_split(filter$options, ",", simplify = TRUE)
    if(length(allowedValues) > 1 || allowedValues != "") {
      checkValues <- y %in% allowedValues
      if(!all(checkValues)) {
        stop(sprintf("Value for filter '%s' not allowed: %s",
                     x, paste(y[!checkValues],collapse = ",")))
      }
    }
    #handle id_list filters
    if(filter$operation == "=,in" && length(y) > 0) {
      return(sprintf("`%s`.`%s` IN (%s)",
                     filter$tableConstraint,
                     filter$field,
                     paste(sprintf("'%s'",y), collapse = ",")))
    }
    #handle everything else
    if(length(y) > 1) {
      #We got a vector with multiple elements,
      #but for a filter which does not allow multiple values (i.e. is not '=,in')
      stop(sprintf("Filter '%s' requires exactly 1 value, %d values given",
                   x, length(y)))
    }
    return(sprintf("`%s`.`%s` %s '%s'",
                   filter$tableConstraint,
                   filter$field, gsub(",in","",filter$operation), y))
  }))

  # Inner Joins ----

  #The most interesting part: We can have up to 3 different types of primary keys / foraign keys:
  # (1) The gene key for joining gene-related tables (e.g. gene_id_<internal_id>_key)
  # (2) The transcript key for joining transcript-related tables (e.g. transcript_id_<internal_id>_key)
  # (2) The translation key for joining translation-related tables (e.g. translation_id_<internal_id>_key)
  #This requires hierarchical joining of tables.

  ###
  # First: join all tables that share a comman primary key / "main" foraing key
  ###

  whereClauses <- unlist(mapply(
    x=mart$.mainTables$table,
    y=mart$.mainTables$primaryKey,
    function(x, y) {
      tableGroup <- targetTables[primaryKey == y,]
      if(nrow(tableGroup) > 1) {
        unlist(lapply(1:(nrow(tableGroup)-1), function(i) {
          sprintf("`%s`.`%3$s` = `%s`.`%3$s`",
                  tableGroup[i,tableConstraint],
                  tableGroup[i+1,tableConstraint],
                  y)
        }))
      }
  }))

  ####
  #Second: Merge tables from different category (gen / transcript / translation)
  ####

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
      fromList <- c(fromList, mainTableOf("transcript"))
      whereClauses <- c(whereClauses,
                        sprintf("`%s`.`%3$s` = `%s`.`%3$s`",
                                mainTableOf("transcript"),
                                firstTableWithPrimaryKeyOf("transcript"),
                                pkOf("transcript")))
    }
    #join transcription main table with any table with a gene id
    whereClauses <- c(whereClauses,
                      sprintf("`%s`.`%3$s` = `%s`.`%3$s`",
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
      fromList <- c(fromList, mainTableOf("translation"))
      whereClauses <- c(whereClauses,
                        sprintf("`%s`.`%3$s` = `%s`.`%3$s`",
                                mainTableOf("translation"),
                                firstTableWithPrimaryKeyOf("translation"),
                                pkOf("translation")))
    }
    #join translation main table with any table with a transcription id
    whereClauses <- c(whereClauses,
                      sprintf("`%s`.`%3$s` = `%s`.`%3$s`",
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
      fromList <- c(fromList, mainTableOf("translation"))
      whereClauses <- c(whereClauses,
                        sprintf("`%s`.`%3$s` = `%s`.`%3$s`",
                                mainTableOf("translation"),
                                firstTableWithPrimaryKeyOf("translation"),
                                pkOf("translation")))
    }
    #join translation main table with any table with a gene id
    whereClauses <- c(whereClauses,
                      sprintf("`%s`.`%3$s` = `%s`.`%3$s`",
                              firstTableWithPrimaryKeyOf("gene"),
                              mainTableOf("translation"),
                              pkOf("gene")))
  }

  # Building final SQL query ----

  query <- sprintf("SELECT %s %s FROM %s",
                   if(uniqueRows) "DISTINCT" else "",
                   paste(attributeList, collapse = ","),
                   paste(fromList, collapse = ","))
  if(length(c(whereClauses, filterList)) > 0) {
    query <- sprintf("%s WHERE %s",
                     query,
                     paste(c(whereClauses, filterList), collapse = " AND "))
  }
  if(verbose) {
    print(query)
  }

  conn <- mart$.conn
  close_connection <- FALSE

  if (is(conn, "EnsemblDBCredentials")) {
    conn <- createDBIConnection(conn)
    close_connection <- TRUE
  }

  result <- dbGetQuery(conn, query)

  if(close_connection)
    dbDisconnect(conn)

  #Remove factor columns and replace them by numeric values whenever possible
  for(col in colnames(result)) {
    result[[col]] <- type.convert(as.character(result[[col]]),as.is=TRUE)
  }
  result
}

getOptionList <- function(optionNodes) {
  paste(extractInfoFromAttributeDescriptions(optionNodes, "value"),collapse = ",")
}

extractInfoFromAttributeDescriptions <- function(nodes, name) {
  sapply(nodes, function(x) xmlGetAttrOrNA(x, name = name))
}

xmlGetAttrOrNA <- function(node, ...) {
  val <- xmlGetAttr(node, ...)
  if(is.null(val)) {
    return(NA)
  }
  val
}

extractListOfBioMartAttributes <- function(dataset, docRoot, mainTables) {
  attributePages <- getNodeSet(docRoot, "//AttributePage[not(@hideDisplay='true')]")
  localAttributes <- NULL
  for (i in 1:length(attributePages)) {
    attrNodes <- getNodeSet(attributePages[[i]], "AttributeGroup/AttributeCollection/AttributeDescription")

    attributes <- data.table(
      internalName = extractInfoFromAttributeDescriptions(attrNodes, "internalName"),
      page = rep(xmlGetAttr(attributePages[[i]], name = "internalName"), length(attrNodes)),
      primaryKey = extractInfoFromAttributeDescriptions(attrNodes, "key"),
      pointerAttribute = extractInfoFromAttributeDescriptions(attrNodes, "pointerAttribute"),
      pointerDataset = extractInfoFromAttributeDescriptions(attrNodes, "pointerDataset"),
      displayName = extractInfoFromAttributeDescriptions(attrNodes, "displayName"),
      tableConstraint = extractInfoFromAttributeDescriptions(attrNodes, "tableConstraint"),
      field = extractInfoFromAttributeDescriptions(attrNodes, "field")
    )
    localAttributes <- rbind(localAttributes, attributes)
  }

  # Replace "main" table references with actual main tables (*_gene__main, *_transcript__main, etc.)
  localAttributes[tableConstraint == "main",
                  tableConstraint := sapply(
                    primaryKey,
                    function(x) mainTables[primaryKey == x, table])
                  ]

  localAttributes
}

extractListOfBioMartFilters <- function(dataset, docRoot, mainTables) {
  filterDescs <- getNodeSet(docRoot, "//FilterCollection[not(@hideDisplay='true')]/FilterDescription")
  localFilters <- NULL
  for (filterDesc in filterDescs) {
    # There exist the following types of filters:
    # "list", "text", "drop_down_basic_filter", "boolean_list", "id_list"
    filterType <- xmlGetAttrOrNA(filterDesc, name = "type")

    # In <FilterDescription> tag with  "boolean_list" and "id_list" as type, every <Option> tag within
    # the <FilterDescription> tag represents a concrete filter
    if(filterType %in% c("boolean_list", "id_list")) {
      nodes <- getNodeSet(filterDesc, "Option")

      filters <- data.table(
        internalName = extractInfoFromAttributeDescriptions(nodes, "internalName"),
        type = rep(filterType, length(nodes)),
        options = sapply(nodes, function(x) getOptionList(getNodeSet(x, "Option"))),
        operation = extractInfoFromAttributeDescriptions(nodes, "legal_qualifiers"),
        primaryKey = extractInfoFromAttributeDescriptions(nodes, "key"),
        pointerAttribute = extractInfoFromAttributeDescriptions(nodes, "pointerAttribute"),
        pointerDataset = extractInfoFromAttributeDescriptions(nodes, "pointerDataset"),
        displayName = extractInfoFromAttributeDescriptions(nodes, "displayName"),
        tableConstraint = extractInfoFromAttributeDescriptions(nodes, "tableConstraint"),
        field = extractInfoFromAttributeDescriptions(nodes, "field")
      )
      localFilters <- rbind(localFilters, filters)
    }
    # In every other case, the <FilterDescription> tag itself represents a filter
    else {
      filters <- data.table(
        internalName = xmlGetAttrOrNA(filterDesc, "internalName"),
        type = filterType,
        options = getOptionList(getNodeSet(filterDesc, "Option")),
        operation = xmlGetAttrOrNA(filterDesc, "legal_qualifiers"),
        primaryKey = xmlGetAttrOrNA(filterDesc, "key"),
        pointerAttribute = xmlGetAttrOrNA(filterDesc, "pointerAttribute"),
        pointerDataset = xmlGetAttrOrNA(filterDesc, "pointerDataset"),
        displayName = xmlGetAttrOrNA(filterDesc, "displayName"),
        tableConstraint = xmlGetAttrOrNA(filterDesc, "tableConstraint"),
        field = xmlGetAttrOrNA(filterDesc, "field")
      )
      localFilters <- rbind(localFilters, filters)
    }
  }

  # Replace "main" table references with actual main tables (*_gene__main, *_transcript__main, etc.)
  localFilters[tableConstraint == "main",
               tableConstraint := sapply(
                 primaryKey,
                 function(x) mainTables[primaryKey == x, table])
               ]
  localFilters
}
