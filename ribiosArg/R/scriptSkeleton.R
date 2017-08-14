#' Generate a Rscript with its skeleton
#'
#' @param file Output file. By default the function writes to standard output.
#'
#' @export
#' @examples
#' scriptSkeleton()

scriptSkeleton <- function(file=stdout()) {
    sentences <- c("#!/usr/bin/env Rscript",
                   "",
                   "# Initialize the script",
                   "suppressMessage(library(ribiosScript))",
                   "ribiosScript::initScript()",
                   "usage <- paste(\"Usage:\", scriptName(), \"USAGES\\nUsage Details\", sep=\"\")",
                   "argParse(\"Op1,Op1Len Op2,Op2Len, ..., log,1\", \
                     \"Param1 Param2\", usage)",
                   "",
                   "# Create log",
                   "logfile <- argGet(\"log\", default=NULL)",
                   "registerLog(logfile)",
                   "doLog(\"%s starts\", scriptName())",
                   "",
                   "# Library loading",
                   "doLog(\"Library loading\")",
                   "libordie(ribiosBase)",
                   "",
                   "# Program logic goes here",
                   "",
                   "",
                   "# Quit",
                   "doLog(\"%s quits\", scriptName())",
                   "qqmsg(status=0L)",
                   sep="\n")
    writeLines(text=sentences, con=file, sep="\n")
    return(invisible(sentences))
}
