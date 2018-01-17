#' Get user ID
#' 
#' @return Character string, user ID
#' @export
#' @examples 
#' getUser()
getUser <- function() system("echo $USER", intern=TRUE)

#' Max design ID
#'
#' @param conn Database connection
#' @return Integer, max design ID
#'
#' @importFrom DBI dbGetQuery
#' @export
maxDesignID <- function(conn) {
  res <- dbGetQuery(conn, "SELECT MAX(ID) AS MAXID FROM Designs")[1,1]
  return(res)
}

#' New design ID
#'
#' @param conn Database connection
#' @return Integer, new design ID (max+1)
#'
#' @export
newDesignID <- function(conn) {
  maxDesignID(conn) + 1L
}

#' Max and new dataset ID
#'
#' @param conn Database connection
#' @return Integer, max dataset ID
#'
#' @importFrom DBI dbGetQuery
#' @export
maxDatasetID <- function(conn) {
  res <- dbGetQuery(conn, "SELECT MAX(ID) AS MAXID FROM Datasets")[1,1]
  if(is.na(res))
      res <- 0L
  return(res)
}

#' New dataset ID
#'
#' @param conn Database connection
#' @return Integer, new dataset ID (max+1)
#'
#' @export
newDatasetID <- function(conn) {
  maxDatasetID(conn) + 1L
}

#' New design ID given an existing datasetID
#'
#' @param conn Database connection
#' @param datasetID An existing datasetID
#'
#' @return New design ID (max+1)
#' @importFrom DBI dbGetQuery
#'
#' @export
newDatasetDesignID <- function(conn, datasetID) {
  res <- dbGetQuery(conn,
                    sprintf("SELECT MAX(DatasetDesignID)+1 AS NEWID FROM Designs WHERE DatasetID=%d", datasetID))[1,1]
  if(is.na(res))
    res <- 1L
  return(res)
}

#' Return max contrast ID
#'
#' @param conn Database connection
#'
#' @export
maxContrastID <- function(conn) {
  res <- dbGetQuery(conn, "SELECT MAX(ID) AS MAXID FROM Contrasts")[1,1]
  if(is.na(res))
      res <- 0L
  return(res)
}

#' Return new contrast IDs
#'
#' @param conn Database connection
#' @param contrastMatrix A contrast matrix, its ncol will be used to extend the IDs
#' 
#' @export
newContrastIDs <- function(conn, contrastMatrix) {
  newID <- maxContrastID(conn)
  res <- seq(from=newID+1, to=newID+ncol(contrastMatrix))
  return(res)
}



#' Assert the correct structure of sampleSubset
#'
#' @param sampleSubset A \code{data.frame}, see details.
#'
#' A valid \code{sampleSubset} is a \code{data.frame} object containing
#' three columns: \code{DatasetSampleIndex} (integer), \code{isUsed} (logical),
#' and \code{Description} (character).
#' Note that the column names are case-sensitive
#'
#' @return Invisible \code{TRUE} if the object is valid, otherwise an error
#' is raised.
#'
#' @export
assertSampleSubset <- function(sampleSubset) {
  stopifnot(is.data.frame(sampleSubset))
  stopifnot(identical(colnames(sampleSubset),
                      c("DatasetSampleIndex", "IsUsed", "Description")))
  return(invisible(TRUE))
}

#' Assert the correct structure of featureSubset
#'
#' @param featureSubset A \code{data.frame}, see details.
#'
#' A valid \code{featureSubset} is a \code{data.frame} object containing
#' three columns: \code{DatasetFeatureIndex} (integer), \code{isUsed} (logical),
#' and \code{Description} (character).
#' Note that the column names are case-sensitive
#'
#' @return Invisible \code{TRUE} if the object is valid, otherwise an error
#' is raised.
#'
#' @export
assertFeatureSubset <- function(featureSubset) {
  stopifnot(is.data.frame(featureSubset))
  stopifnot(identical(colnames(featureSubset),
                      c("DatasetFeatureIndex", "IsUsed", "Description")))
  return(invisible(TRUE))
}

#' Serialize an object and make a blob out of it
#'
#' @param object Any R object
#' @return A blob object that can be unserialized when read from the database
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#' @importFrom blob blob
#' @export
#'
#' @examples
#' myList1 <- list(list(myTeam="HSV", score=16), list(yourTeam="FCB", score=12))
#' myList2 <- list(list(myTeam="HSV", score=19), list(yourTeam="FCB", score=13))
#' myBlob1 <- blobs(myList1)
#' myBlob2 <- blobs(myList2)
#' myDf <- rbind(data.frame(Day=1, Object=myBlob1),
#'               data.frame(Day=2, Object=myBlob2))
#' myCon <- dbConnect(SQLite(), ":memory:")
#' dbWriteTable(conn=myCon, name="testBlobs", value=myDf)
#' myDfOut <- dbReadTable(conn=myCon, name="testBlobs")
#' myList1Out <- unserialize(myDfOut$Object[[1]])
#' myList2Out <- unserialize(myDfOut$Object[[2]])
#' stopifnot(identical(myList1, myList1Out))
#' stopifnot(identical(myList2, myList2Out))

blobs <- function(object) blob(serialize(object, NULL))

#' Serialize a matrix by columns
#'
#' @param matrix A matrix
#'
#' @return A list of serializsed column values
#'
#' @export
#' @examples
#' serializeMatrixByCol(matrix(1:4, ncol=2))
serializeMatrixByCol <- function(matrix) {
  I(lapply(1:ncol(matrix), function(i) serialize(matrix[,i], NULL)))
}

#' Import the content of a EdgeResult object into the database
#' 
#' @param conn Database connection
#' @param edgeResult An object of the class \code{\link[ribiosNGS]{EdgeResult-class}}
#' @param edgeObject An object of EdgeObject
#' @param phenoData An (annotated) data.frame containing sample annotations
#' @param xref External reference
#' @param anno Logical, whether annotation was provided by the edgeR script
#' @param enrichTbl Gene-set enrichment table
#' @param verbose Logical, whether verbose or not
#' 
#' The script is used by ngsDge_edgeR.Rscript to import edgeResult into ROGeR. It is very
#' experimental: the code has not been thoroughly tested and needs refactoring. Use with caution.
#' 
#' @export
#' @importFrom ribiosNGS hasNoReplicate dgeTable featureNames
#' @importFrom ribiosUtils matchColumn trim pScore
#' @importFrom ribiosExpression contrastMatrix designMatrix
#' @importFrom Biobase featureNames
importEdgeResult <- function(conn, edgeResult, edgeObject, 
                             phenoData=NULL, xref=NA, anno=FALSE,
                             enrichTbl, verbose=TRUE) {
  if(anno) {
    annoTarget <- edgeResult@dgeList$annotation
    annotation <- edgeResult@dgeList$genes
    annotation$`_DatasetFeatureIndex` <- 1:nrow(annotation)
    annotation$`_Feature` <- rownames(annotation)
    ga <- readDfFromDb(conn, "GeneAnnotation")
    annotation$`_GeneIndex` <- matchColumn(annotation$GeneID, 
                                           ga, "GeneID")$GeneIndex
  } else {
    annoTarget <- NA
    annotation <- NULL
  }
  
  ## insert into Datasets
  if(verbose)
    message("Writing dataset")
  dsID <- newDatasetID(conn)
  if(is.na(dsID))
    warning("Dataset ID failed - please check the integrity of the database")
  counts <- edgeObject@dgeList$counts
  ds <- data.frame(ID=dsID,
                   Exprs=blobs(counts),
                   ExprsType="count",
                   Pheno=blobs(phenoData),
                   Feature=blobs(annotation),
                   Xref=xref,
                   CreatedBy=getUser(),
                   CreationTime=Sys.time())
  writeDfToDb(conn, ds, tableName="Datasets", row.names=FALSE,
              overwrite = FALSE, append=TRUE)
  
  isUsed <- annotation$`_Feature` %in% featureNames(edgeResult)
  featSubset <- data.frame(DatasetFeatureIndex=annotation$`_DatasetFeatureIndex`,
                           IsUsed=isUsed,
                           Description="At least 1 cpm in N samples where N equals the size of the smallest group")  
  sampleSubset <- data.frame(DatasetSampleIndex=1:ncol(counts),
                             IsUsed=TRUE,
                             Description="ngsDge_edgeR.Rscript currently only support designs where all samples are used.")
  
  ## insert into Designs
  if(verbose)
    message("Writing design")
  newDesID <- newDesignID(conn)
  ## newDDid <- newDatasetDesignID(conn, dsID)
  if(is.na(newDesID))
    warning("Design ID failed - please check the integrity of the database")
  ## if(is.na(newDDid))
  ##  warning("Dataset-specific design ID failed - please check the integrity of the database")
  design <-  data.frame(ID=newDesID,
                        DatasetID=dsID,
                        ## DatasetDesignID=newDDid,
                        Name="defaultDesign",
                        Description="edgeR script default design",
                        SampleSubset=blobs(sampleSubset),
                        FeatureSubset=blobs(featSubset),
                        DesignMatrix=blobs(designMatrix(edgeResult)),
                        CreatedBy=getUser(),
                        CreationTime=Sys.time())
  
  writeDfToDb(conn=conn, design, tableName="Designs", overwrite=FALSE, append=TRUE)
  
  ## insert into DGEmodels
  if(verbose)
    message("Writing DGEmodel")
  dgeModel <- data.frame(DesignID=newDesID,
                         DGEmethodID=2L, ## edgeR
                         InputObj=blobs(edgeResult@dgeList),
                         FitObj=blobs(edgeResult@dgeGLM))
  writeDfToDb(conn=conn, dgeModel, tableName="DGEmodels", overwrite=FALSE, append=TRUE)
  
  ## insert into Contrasts
  if(verbose)
    message("Writing contrasts")
  contMatrix <- contrastMatrix(edgeResult)
  newContIDs <- newContrastIDs(conn, contMatrix)
  if(length(newContIDs)==1 && is.na(newContIDs))
    warning("Contrast ID failed - please check the integrity of the database")
  contrasts <- data.frame(ID=newContIDs,
                          DesignID=newDesID,
                          Name=colnames(contMatrix),
                          Description=colnames(contMatrix),
                          Contrast=serializeMatrixByCol(contMatrix),
                          CreatedBy=getUser(),
                          CreationTime=Sys.time())
  
  writeDfToDb(conn=conn, contrasts, tableName="Contrasts", overwrite=FALSE, append=TRUE)
  
  ## insert to DGEtable
  if(verbose)
    message("Writing DGEtable")
  dgeTbl <- dgeTable(edgeResult)
  dgetable <- data.frame(ContrastID=matchColumn(dgeTbl$Contrast, contrasts, "Name")$ID,
                         FeatureIndex=matchColumn(dgeTbl$GeneID, 
                                                  annotation, "GeneID")$`_DatasetFeatureIndex`,
                         GeneIndex=matchColumn(dgeTbl$GeneID,
                                               annotation, "GeneID")$`_GeneIndex`,
                         AveExprs=dgeTbl$logCPM,
                         Statistic=dgeTbl$LR,
                         LogFC=dgeTbl$logFC,
                         PValue=dgeTbl$PValue,
                         FDR=dgeTbl$FDR, row.names=NULL)
  writeDfToDb(conn=conn, dgetable, tableName="DGEtables", overwrite=FALSE, append=TRUE)
  
  ## insert into GSEtables
  if(verbose)
    message("Writing GSEtable")
  gseMethodId <- ifelse(ribiosNGS::hasNoReplicate(edgeObject), 5L, 1L) ## 5=GAGE, 1=camera
  
  gmtInfo <- readDfFromDb(conn, "RONETgenesets")
  gseContrastID <- match(enrichTbl$Contrast, contrasts$Name)
  gsIndex <- matchColumn(ribiosUtils::trim(as.character(enrichTbl$GeneSet)),
                         gmtInfo, "GeneSetName")$Index
  
  gsetable <- data.frame(GSEmethodID=gseMethodId,
                         ContrastID=gseContrastID,
                         DefaultGenesetID=gsIndex,
                         Correlation=enrichTbl$Correlation,
                         Direction=ifelse(enrichTbl$Direction=="Up", 1L, -1L),
                         PValue=enrichTbl$PValue,
                         FDR=enrichTbl$FDR,
                         EnrichmentScore=pScore(enrichTbl$PValue, sign=enrichTbl$Direction=="Up"))
  
  writeDfToDb(conn=conn, gsetable, tableName="GSEtables", overwrite=FALSE, append=TRUE)
  
  ## results
  res <- list(datasetID=dsID,
              designID=newDesID,
              contrastID=newContIDs)
  return(res)
}
#' Add a new design to an existing Dataset
#'
#' @param conn Database connection
#' @param datasetID The integer ID of an existing dataset
#' @param name Name of the design
#' @param description Description of the design
#' @param sampleSubset A \code{sampleSubset} data.frame
#' @param featureSubset A \code{featureSubset} data.frame
#' @param designMatrix Design matrix, probably returned from \code{model.matrix}
#'
#' @return The design ID is returned. Side-effect: the new design is inserted in
#' to the \code{Designs} table.
#'
#' @importFrom DBI dbGetQuery
#' @export
addDesign <- function(conn,
                      datasetID,
                      name="New design",
                      description=NA,
                      sampleSubset,
                      featureSubset,
                      designMatrix) {
  designId <- newDesignID(conn)
  ## studyDesignId <- newDatasetDesignID(conn, datasetID=datasetID)
  assertSampleSubset(sampleSubset)
  assertFeatureSubset(featureSubset)
  designTbl <- data.frame(ID=designId,
                          DatasetID=datasetID,
                          ## DatasetDesignID=studyDesignId,
                          Name=as.character(name),
                          Description=as.character(description),
                          SampleSubset=blobs(sampleSubset),
                          FeatureSubset=blobs(featureSubset),
                          DesignMatrix=blobs(designMatrix),
                          CreatedBy=getUser(),
                          CreationTime=Sys.time())
  writeDfToDb(conn, designTbl, tableName="Designs", overwrite=FALSE, append=TRUE)
  designId <- dbGetQuery(conn, "SELECT ID FROM Designs WHERE ROWID==LAST_INSERT_ROWID();")[1,1]
  return(designId)
}

removeDesign <- function(conn, designID) {
  sqlComm <- paste("DELETE FROM Designs WHERE ID=", designID, sep="")
  rs <- DBI::dbSendStatement(conn, sqlComm)
  res <- dbGetRowsAffected(rs)
  dbClearResult(rs)
  return(res)
}

#' Add a set of new Contrasts to an existing Design
#'
#' @param conn Database connection
#' @param designID The integer ID of an existing Design
#' @param contrastMatrix The contrast matrix, likely returned by \code{\link[limma]{makeContrasts}}
#' @param names Names of the contrasts. If \code{NULL}, column names are used.
#' @param descriptions Descriptions of the contrasts. Must be the same length
#' as the column number of the contrast matrix. It can contain plain-text
#' descriptions of the contrasts.
#'
#' @return New contrast IDs are returned. Side-effect: the new contrasts are
#'  inserted into the \code{Contrasts} table.
#'
#' @seealso \code{\link[limma]{makeContrasts}}
#'
#' @importFrom DBI dbGetQuery
#' @export
addContrasts <- function(conn,
                         designID,
                         contrastMatrix,
                         names=NULL,
                         descriptions=NULL) {
  if(!is.matrix(contrastMatrix)) {
    contrastMatrix <- matrix(contrastMatrix, ncol=1)
  }
  if(is.null(names) && !is.null(colnames(contrastMatrix))) {
    names <- colnames(contrastMatrix)
  }
  if(is.null(names)) {
    stop("'names' cannot be NULL if the contrastMatrix has no column names!")
  }
  nc <- ncol(contrastMatrix)
  if(!is.null(descriptions)) {
    stopifnot(length(descriptions)==nc)
  } else {
    descriptions <- rep(NA, nc)
  }
  lastContrastID <- dbGetQuery(conn, "SELECT MAX(ID) FROM Contrasts")[1,1]
  newContrastIDs <- seq(from=lastContrastID+1,to=lastContrastID+nc)
  newContrasts <- data.frame(ID=newContrastIDs,
                             DesignID=designID,
                             Name=names,
                             Description=descriptions,
                             Contrast=serializeMatrixByCol(contrastMatrix),
                             CreatedBy=getUser(),
                             CreationTime=Sys.time())
  writeDfToDb(conn, newContrasts, "Contrasts", overwrite=FALSE, append=TRUE)
  return(newContrastIDs)
}

#' @include df.R
