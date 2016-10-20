QSELECT_COMMAND <- file.path(file.path(system.file(package="ribiosBatchJobs"), "exec", "qselect-rbalhpc05"))
QSUB_COMMAND <- file.path(file.path(system.file(package="ribiosBatchJobs"), "exec", "qsub-rbalhpc05"))
QDEL_COMMAND <- file.path(file.path(system.file(package="ribiosBatchJobs"), "exec", "qdel-rbalhpc05"))

#' makeClusterFunctions for remote torque
#'
#' @param template.file the same as in makeClusterFunctionTorque
#' @param list.jobs.cmd the same as in makeClusterFunctionTorque
#'
#' The function extends makeClusterFunctions in BatchJobs. It allows submitting jobs to a remote server where the Torque/PBS system is installed, via ssh.
#'
#' @export
#' @importFrom checkmate assertCharacter
#' @importFrom ribiosUtils trim
#' @importFrom BBmisc collapse

makeClusterFunctionsRemoteTorque <- function (template.file,
                                              list.jobs.cmd = c(QSELECT_COMMAND, "-u $USER", "-s EHQRTW")) 
{
    checkmate::assertCharacter(list.jobs.cmd, min.len = 1L, any.missing = FALSE)
    template = cfReadBrewTemplate(template.file)
    submitJob = function(conf, reg, job.name, rscript, log.file, 
        job.dir, resources, arrayjobs) {
        outfile = ribiosCfBrewTemplate(conf, template, rscript, "pbs")
        res = BatchJobs:::runOSCommandLinux(QSUB_COMMAND, 
            outfile, stop.on.exit.code = FALSE)
        max.jobs.msg = "Maximum number of jobs already in queue"
        output = collapse(res$output, sep = "\n")
        if (grepl(max.jobs.msg, output, fixed = TRUE)) {
            makeSubmitJobResult(status = 1L, batch.job.id = NA_character_, 
                msg = max.jobs.msg)
        }
        else if (res$exit.code > 0L) {
            cfHandleUnknownSubmitError(QSUB_COMMAND, res$exit.code, 
                res$output)
        }
        else {
            makeSubmitJobResult(status = 0L, batch.job.id = ribiosUtils::trim(output))
        }
    }
    killJob = function(conf, reg, batch.job.id) {
        cfKillBatchJob(QDEL_COMMAND, batch.job.id)
    }
    listJobs = function(conf, reg) {
        batch.ids = BatchJobs:::runOSCommandLinux(list.jobs.cmd[1L], list.jobs.cmd[-1L])$output
        unique(gsub("\\[[[:digit:]]\\]", "[]", batch.ids))
    }
    getArrayEnvirName = function() "PBS_ARRAYID"
    makeClusterFunctions(name = "RemoteTorque", submitJob = submitJob, 
        killJob = killJob, listJobs = listJobs, getArrayEnvirName = getArrayEnvirName)
}
