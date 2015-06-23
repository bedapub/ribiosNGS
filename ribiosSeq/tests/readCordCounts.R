getTestFile <- function(filename) {
  installed.filename <- system.file("extdata", filename, package="ribiosSeq")
  if(all(file.exists(installed.filename)))
    return(installed.filename)

  local.filename <- file.path("..", "inst", "extdata", filename)
  if(all(file.exists(local.filename)))
    return(local.filename)

  stop("File not found\n")
}

library(ribiosSeq)
count.files <- dir("/DATA/bi/apps/ngs/cynomolgus/cyno_wta_solid0365_20110107_FRAG/R-plot/chr20", pattern="\\.counts", full.names=TRUE)
system.time(rc <- readCoordCounts(inputDir="/DATA/bi/apps/ngs/cynomolgus/cyno_wta_solid0365_20110107_FRAG/R-plot/chr20",pattern="\\.counts$"))
system.time(ccranges <- readCoordCountRange(inputDir="/DATA/bi/apps/ngs/cynomolgus/cyno_wta_solid0365_20110107_FRAG/R-plot/chr20",pattern="\\.counts$"))
