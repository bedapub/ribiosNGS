#' Check sample annotation data.frame or tibble meets the requirement of the
#' biokit pipeline
#' 
#' 
#' @param df Sample annotation, can be either a \code{data.frame} or
#' \code{tbl_df} object
#' @return Invisible \code{NULL} if the requirement is met, otherwise an error
#' is printed and the function stops
#' 
#' The biokit pipeline requires that values in each column contain no empty
#' spaces. This function ensures that.
#' @seealso \code{\link{writeBiokitSampleAnnotation}}, which calls this
#' function to ensure that the sample annotation file is ok
#' @examples
#' 
#' test <- data.frame(Char=LETTERS[1:6],
#'                    Integer=1:6,
#'                    Number=pi*1:6,
#'                    Factor=gl(2,3, labels = c("level 1", "level 2")), stringsAsFactors=FALSE)
#' testthat::expect_error(checkBiokitSampleAnnotation(test),
#'                        regexp = "level 1.*level 2")
#' testFix <- data.frame(Char=LETTERS[1:6],
#'                       Integer=1:6,
#'                       Number=pi*1:6,
#'                       Factor=gl(2,3, labels = c("level1", "level2")), stringsAsFactors=FALSE)
#' testthat::expect_silent(checkBiokitSampleAnnotation(testFix))
#' if(require("tibble")) {
#'   testthat::expect_error(checkBiokitSampleAnnotation(tibble::as_tibble(test)),
#'                          regexp = "level 1.*level 2")
#'   testthat::expect_silent(checkBiokitSampleAnnotation(tibble::as_tibble(testFix)))
#' }
#' 
#' @export checkBiokitSampleAnnotation
checkBiokitSampleAnnotation <- function(df) {
  classes <- df %>% dplyr::summarise_all(class) 
  for(i in seq(along=classes)) {
    currClass <- classes[i]
    if(currClass %in% c("factor", "character")) {
      if(currClass=="character") {
        currVals <- as.character(df[,i,drop=TRUE])
      } else if (currClass=="factor") {
        currVals <- levels(df[,i,drop=TRUE])
      } else {
        stop("Should not be here. Contact the developmer")
      }
      if(any(grepl("\\s", currVals))) {
        msg <- paste0("Empty characters detected in the column '",
                      colnames(df)[i],
                      "', here are the first few values:",
                      paste0(head(grep("\\s", currVals, value=TRUE)), collapse=","),
                      "\nEmpty space in the annotation file is not supported by biokit. Please modify the object!")
        stop(msg)
      }
    }
  }
  return(invisible(NULL))
}


#' Write sample annotation into a tab-delimited file to start the Biokit
#' pipeline
#' 
#' 
#' @param df A data.frame or anything that can be converted to a data.frame
#' @param con Connection, can be a character string indicating file name
#' @return NULL, side effect is used
#' @note Starting from version 1.0-36, the function checks the input
#' \code{data.frame} or \code{tbl_df} before writing to the file
#' @examples
#' 
#' testDf <- data.frame(ID=LETTERS[1:4], 
#'    GROUP=gl(2,2), 
#'    FASTQ1=sprintf("%s_R1.fastq.gz", LETTERS[1:4]),
#'    FASTQ2=sprintf("%s_R1.fastq.gz", LETTERS[1:4]))
#' tmp <- tempfile()
#' writeBiokitSampleAnnotation(testDf, con=tmp)
#' readLines(tmp)
#' 
#' @export writeBiokitSampleAnnotation
writeBiokitSampleAnnotation <- function(df, con) {
  checkBiokitSampleAnnotation(df)
  firstLine <- paste0("#", paste(colnames(df), collapse="\t"))
  writeLines(firstLine, con=con)
  readr::write_tsv(df, path=con, append=TRUE)
}
