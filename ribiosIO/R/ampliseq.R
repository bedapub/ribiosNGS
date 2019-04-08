find_ampliseq <- function(dir) {
  files <- dir(dir, pattern="*.cov.xls", full.names=TRUE, recursive=TRUE)
  finfo <- file.info(files)
  isValid <- !is.na(finfo$size) & finfo$size>0
  files[isValid]
}

read_ampliseq <- function(files) {
  tbls <- lapply(files, function(x) {
    tbl <- read.table(x, sep="\t", header=TRUE)
    return(tbl[,c("attributes", "total_reads")])
  })
  uniqGenes <- unique(as.vector(sapply(tbls, function(x) x$attributes)))
  ntbls <- do.call(cbind, lapply(tbls, function(x) {
    x$total_reads[match(uniqGenes, x$attributes)]
    ## matchColumn(uniqGenes, x, "attributes")$total_reads))
  }))
  rownames(ntbls) <- gsub("GENE_ID=", "", uniqGenes)
  colnames(ntbls) <- basename(dirname(files))
  return(ntbls)
}

find_and_read_ampliseq <- function(dir) {
  files <- find_ampliseq(dir)
  read_ampliseq(files)
}


#' Read AmpliSeq amplicon informaiton from a BED file
#' @param bedFile Character string, a BED file
#' @return A \code{data.frame} containing following information: Amplicon, GeneSymbol, RefSeq, Length
#' 
#' The function is used to read Amplicon information from BED file
#' 
#' @examples 
#' lines <- paste0("#track type=bedDetail ionVersion=4.0 name=\"IAD50039-4_IAD87652-4_Design\"",
#'  "solution_type=4 description=\"TargetRegions_AmpliSeqID_IAD50039 AmpliSeq_Version=3.0.1",
#'  " Workflow=RNA merged with TargetRegions_AmpliSeqID_IAD87652 AmpliSeq_Version=4.48 Workflow=RNA\"",
#'  " color=77,175,74 priority=2", "\n",
#'  "NM_000014\t3316\t3421\tAMPL1384\t0\t+\t.\tGENE_ID=A2M", "\n",
#'  "NM_005502\t2488\t2589\tAMPL28385508\t0\t+\t.\tGENE_ID=ABCA1","\n",
#'  "NM_000927\t2520\t2624\tAMPL5599607\t0\t+\t.\tGENE_ID=ABCB1","\n",
#'  "NM_000443\t1367\t1470\tAMPL5513474\t0\t+\t.\tGENE_ID=ABCB4")
#'  read_ampliseq_amplicons(textConnection(lines))
read_ampliseq_amplicons <- function(bedFile) {
  ampliconsRaw <- read.table(bedFile)
  amplicons <- data.frame(Amplicon=ampliconsRaw$V4,
                          GeneSymbol=sub("GENE_ID=", "", ampliconsRaw$V8),
                          RefSeq=ampliconsRaw$V1,
                          Length=abs(ampliconsRaw$V3-ampliconsRaw$V2),
                          stringsAsFactors = FALSE)
  return(amplicons)
}

#' Read bedcov output of AmpliSeq amplicons and convert them to read counts
#' @param file Character string, a GCT file containing bedcov output of amplicons
#' @param bedFile Character string, a BED file encoding amplicons
#' @return A \code{GctMatrix} object containing read counts
#' 
#' The function is used to convert read base counts returned by \code{samtools bedcov} to read counts using Amplicon information encoded in the bed file
#' 
#' @examples
#' bedlines <- paste0("#track type=bedDetail ionVersion=4.0 name=\"IAD50039-4_IAD87652-4_Design\"",
#'  "solution_type=4 description=\"TargetRegions_AmpliSeqID_IAD50039 AmpliSeq_Version=3.0.1",
#'  " Workflow=RNA merged with TargetRegions_AmpliSeqID_IAD87652 AmpliSeq_Version=4.48 Workflow=RNA\"",
#'  " color=77,175,74 priority=2", "\n",
#'  "NM_000014\t3316\t3421\tAMPL1384\t0\t+\t.\tGENE_ID=A2M", "\n",
#'  "NM_005502\t2488\t2589\tAMPL28385508\t0\t+\t.\tGENE_ID=ABCA1","\n",
#'  "NM_000927\t2520\t2624\tAMPL5599607\t0\t+\t.\tGENE_ID=ABCB1","\n",
#'  "NM_000443\t1367\t1470\tAMPL5513474\t0\t+\t.\tGENE_ID=ABCB4")
#'  gctLines <- paste0("#1.2", "\n",
#'  "3\t3","\n",
#'  "NAME\tDescription\tS1\tS2\tS3","\n",
#'  "A2M\tNM_000014\t105\t210\t315", "\n",
#'  "ABCA1\tNM_005502\t202\t303\t404", "\n",
#'  "ABCB1\tNM_000927\t312\t416\t520")
#'  read_ampliseq_bedcovgct(textConnection(gctLines), textConnection(bedlines))
read_ampliseq_bedcovgct <- function(file,
                                     bedFile) {
  amplicons <- read_ampliseq_amplicons(bedFile)
  
  if(is(file, "textConnection")) {
    baseCount <- read_gctstr_matrix(paste0(readLines(file), collapse="\n"))
  } else {
    baseCount <- read_gct_matrix(file)
  }
  baseCountNormDf <- matchColumn(gctDesc(baseCount),
                                 amplicons, "RefSeq")
  stopifnot(identical(baseCountNormDf$GeneSymbol,
                      rownames(baseCount)))
  baseCountNormFactor <- baseCountNormDf$Length
  stopifnot(!any(is.na(baseCountNormFactor)))
  count <- round(baseCount/baseCountNormFactor)
  return(count)
}
