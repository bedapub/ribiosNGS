#' Write sample annotation into a tab-delimited file to start the Biokit pipeline
#' @param df A data.frame or anything that can be converted to a data.frame
#' @param con Connection, can be a character string indicating file name
#' 
#' @return NULL, side effect is used
#' 
#' @examples 
#' testDf <- data.frame(ID=LETTERS[1:4], 
#'    GROUP=gl(2,2), 
#'    FASTQ1=sprintf("%s_R1.fastq.gz", LETTERS[1:4]),
#'    FASTQ2=sprintf("%s_R1.fastq.gz", LETTERS[1:4]))
#' tmp <- tempfile()
#' writeBiokitSampleAnnotation(testDf, con=tmp)
#' readLines(tmp)
writeBiokitSampleAnnotation <- function(df, con) {
  firstLine <- paste0("#", paste(colnames(df), collapse="\t"))
  writeLines(firstLine, con=con)
  readr::write_tsv(df, path=con, append=TRUE)
}
