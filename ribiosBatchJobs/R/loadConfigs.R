#' Load config for the bioinfo server (or any other server than rbalhpc05)
#' @export
loadConfigBioinfo <- function() {
    BatchJobs::loadConfig(system.file("templates",
                           "BatchJobs-bioinfo.R",
                           package="ribiosBatchJobs"))
}

#' Load config for the rbalhpc05 server (which runs Torque/PBS)
#' @export
loadConfigRbalhpc05 <- function() {
    BatchJobs::loadConfig(system.file("templates",
                           "BatchJobs-rbalhpc05.R",
                           package="ribiosBatchJobs"))
}
