#' Read the Data block of Illumina sample sheet as data.frame
#' @param file An Illumina SampleSheet, with one Data block
#' @return A \code{data.frame} of the data block
#' 
#' @examples 
#' myText <- paste("[Header]",
#'   "IEMFileVersion,5",
#'   "",
#'   "[Reads]",
#'   "51",
#'   "1",
#'   "[Data]",
#'   "Lane,Sample_ID,Description",
#'   "1,1,Sample1",
#'   "1,2,Sample2",
#'   "2,3,Sample3",
#'   "2,4,Sample4", sep="\n")
#' read_illumina_sampleSheet(textConnection(myText))
read_illumina_sampleSheet <- function(file) {
  lines <- readLines(file)
  dataBlock <- grep("^\\[Data\\]", lines)
  if(length(dataBlock)==0)
    stop("No data block found")
  res <- read.csv(textConnection(lines), header=TRUE, sep=",", 
                  quote="", dec=".", row.names=NULL, 
                  stringsAsFactors = FALSE,
                  skip=dataBlock)
  return(res)
}
