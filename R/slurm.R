#' Check a contrast matrix to make sure that it is likely o.k.
#' @param contrastMatrix A contrast matrix
#' @param action Character strings, the action to perform in case the names show irregularities
#'
#' Right now, the function checks no column names contain the equal sign.
#'
#' @examples
#' testDesign <- cbind(Control=rep(1,8), Treatment=rep(c(0,1),4), Batch=rep(c(0, 1), each=4))
#' problemContrast <- limma::makeContrasts("Treatment"="Treatment",
#'   "Batch=Batch", ## problematic
#'   levels=testDesign)
#' checkContrastNames(problemContrast, action="message")
#' if(requireNamespace("testthat")) {
#'   testthat::expect_warning(checkContrastNames(problemContrast,
#'          action="warning"))
#'   testthat::expect_error(checkContrastNames(problemContrast,
#'          action="error"))
#' }
#' @export
checkContrastNames <- function(contrastMatrix,
                                action=c("message", "warning", "error")) {
  action <- match.arg(action)
  hasEqual <- grepl("=", colnames(contrastMatrix))
  if(any(hasEqual)) {
    msg <- paste("Some columns in the contrast matrix contain equal signs '='. ",
                 "This is often caused by setting contrasts in the form of ",
                 "\"BvC=B-C\" instead of \"BvC\"=\"B-C\". ",
                 "The problematic columns include:\n",
                 paste(colnames(contrastMatrix)[hasEqual], sep = "", collapse = "\n"),
                 sep="")
    if(action=="message") {
      message(msg)
    } else if (action=="warning") {
      warning(msg)
    } else if (action=="error") {
      stop(msg)
    }
  }
  return(invisible(NULL))
}

#' Export an DGEList, designMatrix, and contrastMatrix to files and return the
#' command to run the edgeR script
#'
#' @param dgeList An \code{DGEList} object with \code{counts}, \code{genes},
#' and \code{samples}
#' @param designMatrix The design matrix to model the data
#' @param contrastMatrix The contrast matrix matching the design matrix
#' @param outdir Output directory of the edgeR script. Default value
#' "edgeR_output".
#' @param outfilePrefix Prefix of the output files, for instance a reasonable
#' name of the project, to identify the files uniquely. The files will be written in
#' \code{file.path(OUTDIR, 'input_data')}.
#' @param mps Logical, whether molecular-phenotyping analysis is run.
#' @param limmaVoom Logical, whether the limma-voom model is run instead of the edgeR model
#' @param appendGmt \code{NULL} or character string, path to an additional
#'   GMT file besides the default GMT file used to perform gene-set analysis.
#'   The GMT file must exist.
#' @param debug Logical, if \code{TRUE}, the source code of Rscript is used
#'   instead of the installed version.
#' @param rootPath Character, the root path of the script
#' @param contrastAnno A \code{data.frame} or \code{NULL}, contrast annotation.
#'
#' @note Following checks are done internally: \itemize{ \item The design
#' matrix must have the same number of rows as the columns of the count matrix.
#' \item The contrast matrix must have the same number of rows as the columns
#' of the design matrix.  \item Row names of the design matrix match the column
#' names of the expression matrix. In case of suspect, the program will stop
#' and report. }
#'
#' The output file names start with the outfilePrefix, followed by '-' and
#' customed file suffixes.
#' @examples
#'
#'  mat <- matrix(rnbinom(100, mu=5, size=2), ncol=10)
#'  rownames(mat) <- sprintf("gene%d", 1:nrow(mat))
#'  myFac <- gl(2,5, labels=c("Control", "Treatment"))
#'  y <- edgeR::DGEList(counts=mat, group=myFac)
#'  myDesign <- model.matrix(~myFac); colnames(myDesign) <- levels(myFac)
#'  myContrast <- limma::makeContrasts(Treatment, levels=myDesign)
#'  edgeRcommand(y, designMatrix=myDesign, contrastMatrix=myContrast,
#'      outfilePrefix="test", outdir=tempdir())
#'
#' @importFrom ribiosUtils haltifnot createDir assertFile trim
#' @importFrom ribiosIO writeMatrix
#' @export edgeRcommand
edgeRcommand <- function(dgeList, designMatrix, contrastMatrix,
                         outdir="edgeR_output",
                         outfilePrefix="an-unnamed-project-",
                         mps=FALSE,
                         limmaVoom=FALSE,
                         appendGmt=NULL,
                         debug=FALSE,
                         rootPath="/pstore/apps/bioinfo/geneexpression/",
                         contrastAnno=NULL) {
  ## remove trailing -s if any
  outfilePrefix <- gsub("-$", "", outfilePrefix)
  outfileWithDir <- file.path(outdir,
                              'input_data',
                              basename(outfilePrefix))

  ## check consistency between names
  exprsMat <- dgeList$counts
  if(!identical(rownames(designMatrix), colnames(exprsMat)) &&
     (is.null(rownames(designMatrix)) || identical(rownames(designMatrix),
                                                   as.character(1:nrow(designMatrix))))) {
    rownames(designMatrix) <- colnames(exprsMat)
  }
  haltifnot(nrow(designMatrix) == ncol(exprsMat),
            msg="The design matrix must have the same number of rows as the columns of the count matrix.")
  haltifnot(identical(rownames(designMatrix), colnames(exprsMat)),
            msg="Row names of the design matrix not matching column names of the expression matrix.")
  haltifnot(ncol(designMatrix) == nrow(contrastMatrix),
            msg="The contrast matrix must have the same number of rows as the columns of the design matrix.")
  checkContrastNames(contrastMatrix, action="error")

  if(!is.null(appendGmt)) {
    ribiosUtils::assertFile(appendGmt)
    appendGmtComm <- sprintf("-appendGmt %s", appendGmt)
  } else {
    appendGmtComm <- ""
  }

  ribiosUtils::createDir(dirname(outfileWithDir), recursive=TRUE, mode="0770")

  exprsFile <- paste0(outfileWithDir, "-counts.gct")
  tpmFile <- paste0(outfileWithDir, "-tpm.gct")
  fDataFile <- paste0(outfileWithDir, "-featureAnno.txt")
  pDataFile <- paste0(outfileWithDir, "-sampleAnno.txt")
  groupFile <- paste0(outfileWithDir, "-sampleGroups.txt")
  groupLevelFile <- paste0(outfileWithDir, "-sampleGroupLevels.txt")
  designFile <- paste0(outfileWithDir, "-designMatrix.txt")
  contrastFile <- paste0(outfileWithDir, "-contrastMatrix.txt")
  contrastAnnoFile <- paste0(outfileWithDir, "-contrastAnnotation.txt")
  
  writeDGEList(dgeList, exprs.file=exprsFile,
               fData.file = fDataFile,
               pData.file = pDataFile,
               group.file = groupFile,
               groupLevels.file = groupLevelFile)
  writeMatrix(designMatrix, designFile)
  writeMatrix(contrastMatrix, contrastFile)
  if(!is.null(contrastAnno)) {
    writeMatrix(contrastAnno, contrastAnnoFile)
  }

  logFile <- paste0(gsub("\\/$", "", outdir), ".log")
  mpsComm <- ifelse(mps, "-mps", "")
  limmaVommComm <- ifelse(limmaVoom, "-limmaVoom", "")
  commandFile <- paste0(outfileWithDir, "-edgeRcommand.txt")

  scriptFile <- file.path(rootPath,
                         ifelse(debug, "rsrc", "bin"),
                         "ngsDge_edgeR.Rscript")

  command <- paste(scriptFile,
                   sprintf("-infile %s", exprsFile),
                   sprintf("-designFile %s", designFile),
                   sprintf("-contrastFile %s", contrastFile),
                   sprintf("-sampleGroups %s", groupFile),
                   sprintf("-groupLevels %s", groupLevelFile),
                   sprintf("-featureAnnotationFile %s", fDataFile),
                   sprintf("-phenoData %s", pDataFile),
                   sprintf("-outdir %s", outdir),
                   sprintf("-log %s", logFile),
                   sprintf("-writedb"),
                   appendGmtComm,
                   mpsComm,
                   limmaVommComm)
  command <- ribiosUtils::trim(gsub("\\s+", " ", command))

  writeLines(command, con=commandFile)
  return(command)
}

#' Return the SLURM command to run the edgeR script
#'
#' @param dgeList An \code{DGEList} object with \code{counts}, \code{genes},
#' and \code{samples}
#' @param designMatrix The design matrix to model the data
#' @param contrastMatrix The contrast matrix matching the design matrix
#' @param outfilePrefix Prefix of the output files. It can include directories,
#' e.g. \code{"data/outfile-"}. In case of \code{NULL}, temporary files will be
#' created.
#' @param outdir Output directory of the edgeR script. Default value
#' "edgeR_output".
#' @param mps Logical, whether molecular-phenotyping analysis is run.
#' @param limmaVoom Logical, whether the limma-voom model is run instead of the edgeR model
#' @param appendGmt \code{NULL} or character string, path to an additional GMT
#'   file for gene-set analysis. The option is passed to
#'   \code{\link{edgeRcommand}}.
#' @param qos Character, specifying Quality of Service of Slurm. Available values include \code{short} (recommended default, running time cannot exceed 3 hours), \code{interactive} (useful if you wish to get the results from an interactive session), and \code{normal} (useful if the job is expected to run more than three hours.)
#' using \code{srun} and the 'interaction' queue of jobs instead of using
#' \code{sbatch}.
#' @param params Character, further parameters to pass to sbatch, for instance "--partition ANOTHER_PARITION"
#' @param debug Logical, if \code{TRUE}, the source code of Rscript is used instead of
#'   the installed version. The option is passed to \code{edgeRcommand}.
#'
#' This function wraps the function \code{\link{edgeRcommand}} to return the
#' command needed to start a SLURM job.
#'
#' It uses \code{outdir} to specify slurm output and error files as in the same
#' directory of \code{outdir}. And the job name is set as the name of the
#' output directory.
#' @seealso \code{\link{edgeRcommand}}
#' @examples
#'
#'  mat <- matrix(rnbinom(100, mu=5, size=2), ncol=10)
#'  rownames(mat) <- sprintf("gene%d", 1:nrow(mat))
#'  myFac <- gl(2,5, labels=c("Control", "Treatment"))
#'  y <- edgeR::DGEList(counts=mat, group=myFac)
#'  myDesign <- model.matrix(~myFac); colnames(myDesign) <- levels(myFac)
#'  myContrast <- limma::makeContrasts(Treatment, levels=myDesign)
#'  slurmEdgeRcommand(y, designMatrix=myDesign, contrastMatrix=myContrast,
#'      outfilePrefix="test", outdir=tempdir())
#'
#' @export slurmEdgeRcommand
slurmEdgeRcommand <- function(dgeList, designMatrix, contrastMatrix,
                              outdir="edgeR_output",
                              outfilePrefix="an-unnamed-project-",
                              mps=FALSE,
                              limmaVoom=FALSE,
                              appendGmt=NULL,
                              qos=c("short", "interactive", "normal"),
                              params = "",
                              debug=FALSE) {
  qos <- match.arg(qos)
  comm <- edgeRcommand(dgeList=dgeList,
                       designMatrix=designMatrix,
                       contrastMatrix=contrastMatrix,
                       outdir=outdir,
                       outfilePrefix=outfilePrefix,
                       mps=mps,
                       limmaVoom=limmaVoom,
                       appendGmt=appendGmt,
                       debug=debug)
  outdirBase <- basename(gsub("\\/$", "", outdir))
  outfile <- file.path(dirname(outdir), paste0("slurm-", outdirBase, ".out"))
  errfile <- file.path(dirname(outdir), paste0("slurm-", outdirBase, ".err"))
  if(qos == "short") {
    prefix <- "sbatch --qos=short"
  } else if (qos=="interactive") {
    prefix <- "srun --qos=interaction"
  } else if (qos=="normal") {
    prefix <- "sbatch --qos=normal"
  } else {
    stop("Should not be here")
  }

  res <- paste(prefix,
               "-n 1 -c 12",
               sprintf("-e %s", errfile),
               sprintf("-J %s", outdirBase),
               sprintf("-o %s", outfile),
               params,
               comm)
  return(res)
}

#' Return the LSF command to run the edgeR script
#'
#' @param dgeList An \code{DGEList} object with \code{counts}, \code{genes},
#' and \code{samples}
#' @param designContrast The DesignContrast object to model the data
#' @param outfilePrefix Prefix of the output files. It can include directories,
#' e.g. \code{"data/outfile-"}. In case of \code{NULL}, temporary files will be
#' created.
#' @param outdir Output directory of the edgeR script. Default value
#' "edgeR_output".
#' @param mps Logical, whether molecular-phenotyping analysis is run.
#' @param limmaVoom Logical, whether the limma-voom model is run instead of the edgeR model
#' @param appendGmt \code{NULL} or character string, path to an additional GMT
#'   file for gene-set analysis. The option is passed to
#'   \code{\link{edgeRcommand}}.
#' @param qos Character, specifying Quality of Service of LSF Available values include \code{long}, \code{short}, and \code{preempty}.
#' @param debug Logical, if \code{TRUE}, the source code of Rscript is used instead of
#'   the installed version. The option is passed to \code{edgeRcommand}.
#'
#' This function wraps the function \code{\link{edgeRcommand}} to return the
#' command needed to start a LSF job.
#'
#' It uses \code{outdir} to specify slurm output and error files as in the same
#' directory of \code{outdir}. And the job name is set as the name of the
#' output directory.
#'
#' @seealso \code{\link{edgeRcommand}}
#' @examples
#'
#'  mat <- matrix(rnbinom(100, mu=5, size=2), ncol=10)
#'  rownames(mat) <- sprintf("gene%d", 1:nrow(mat))
#'  myFac <- gl(2,5, labels=c("Control", "Treatment"))
#'  y <- edgeR::DGEList(counts=mat, group=myFac)
#'  myDesign <- model.matrix(~myFac); colnames(myDesign) <- levels(myFac)
#'  myContrast <- limma::makeContrasts(Treatment, levels=myDesign)
#'  myDesCon <- DesignContrast(designMatrix=myDesign, contrastMatrix=myContrast)
#'  lsfEdgeRcommand(y, designContrast=myDesCon,
#'      outfilePrefix="test", outdir=tempdir())
#'  ## remove the bsub file
#'  file.remove("test.bsub")
#'
#' @importFrom ribiosExpression contrastAnnotation
#' @export
lsfEdgeRcommand <- function(dgeList, designContrast,
                            outdir="edgeR_output",
                            outfilePrefix="an-unnamed-project-",
                            mps=FALSE,
                            limmaVoom=FALSE,
                            appendGmt=NULL,
                            qos=c("long", "preempty","short"),
                            debug=FALSE) {
  qos <- match.arg(qos)
  comm <- edgeRcommand(dgeList=dgeList,
                       designMatrix=designMatrix(designContrast),
                       contrastMatrix=contrastMatrix(designContrast),
                       outdir=outdir,
                       outfilePrefix=outfilePrefix,
                       mps=mps,
                       limmaVoom=limmaVoom,
                       appendGmt=appendGmt,
                       debug=debug,
                       rootPath="/projects/site/pred/beda/apps/geneexpression",
                       contrastAnno = ribiosExpression::contrastAnnotation(designContrast))
  outdirBase <- basename(gsub("\\/$", "", outdir))
  outfile <- file.path(dirname(outdir), paste0("lsf-", outdirBase, ".out"))
  errfile <- file.path(dirname(outdir), paste0("lsf-", outdirBase, ".err"))
  bsub <- c("#!/bin/bash",
            sprintf("#BSUB -J edgeR_%s ## job name", outfilePrefix),
            "#BSUB -n 12 ## processors on serial clusters",
            ## "#BSUB -c 12 ## number of CPUs",
            sprintf("#BSUB -q %s ##short (3h)/long(15d)/preempt(no limit)", qos),
            sprintf("#BSUB -o %s-%%J.out ## output file", outfilePrefix),
            sprintf("#BSUB -e %s-%%J.err ## error file", outfilePrefix),
            comm)
  bsubFile <- paste0(gsub("-$", "", outfilePrefix), ".bsub")
  writeLines(bsub, bsubFile)

  res <- paste0("ml load .testing; ml load R/4.0.5-foss-2020a; bsub < ", bsubFile)
  return(res)
}

#' Send an edgeR analysis job to SLURM
#'
#' @param dgeList An \code{DGEList} object with \code{counts}, \code{genes},
#' and \code{samples}
#' @param designMatrix The design matrix to model the data
#' @param contrastMatrix The contrast matrix matching the design matrix
#' @param outfilePrefix Prefix of the output files. It can include directories,
#' e.g. \code{"data/outfile-"}. In case of \code{NULL}, temporary files will be
#' created.
#' @param outdir Output directory of the edgeR script. Default value
#' "edgeR_output".
#' @param overwrite If \code{ask}, the user is asked before an existing output
#' directory is overwritten. If \code{yes}, the job will start and an existing
#' directory will be overwritten anyway. If \code{no}, and if an output
#' directory is present, the job will not be started.
#' @param mps Logical, whether molecular-phenotyping analysis is run.
#' @param limmaVoom Logical, whether the limma-voom model is run instead of the edgeR model.
#' @param appendGmt \code{NULL} or character string, path to an additional GMT
#'   file for gene-set analysis. The option is passed to
#'   \code{\link{slurmEdgeRcommand}} and then to \code{\link{edgeRcommand}}.
#' @param qos Character, specifying Quality of Service of Slurm. Available values include \code{short} (recommended default, running time cannot exceed 3 hours), \code{interactive} (useful if you wish to get the results from an interactive session), and \code{normal} (useful if the job is expected to run more than three hours.)
#' using \code{srun} and the 'interaction' queue of jobs instead of using
#' \code{sbatch}.
#' @param debug Logical, if \code{TRUE}, the source code of Rscript is used instead of
#'   the installed version. The option is passed to \code{edgeRcommand}.
#' @return A list of two items, \code{command}, the command line call, and
#' \code{output}, the output of the SLURM command in bash
#' @note Even if the output directory is empty, if \code{overwrite} is set to
#' \code{no} (or if the user answers \code{no}), the job will not be started.
#' @examples
#'
#'  mat <- matrix(rnbinom(100, mu=5, size=2), ncol=10)
#'  rownames(mat) <- sprintf("gene%d", 1:nrow(mat))
#'  myFac <- gl(2,5, labels=c("Control", "Treatment"))
#'  y <- edgeR::DGEList(counts=mat, group=myFac)
#'  myDesign <- model.matrix(~myFac); colnames(myDesign) <- levels(myFac)
#'  myContrast <- limma::makeContrasts(Treatment, levels=myDesign)
#'  ## \dontrun{
#'  ## slurmEdgeR(y, designMatrix=myDesign, contrastMatrix=myContrast,
#'  ##  outfilePrefix="test", outdir=tempdir())
#'  ## }
#'
#' @export slurmEdgeR
slurmEdgeR <- function(dgeList, designMatrix, contrastMatrix,
                       outdir="edgeR_output",
                       outfilePrefix="an-unnamed-project-",
                       overwrite=c("ask", "yes", "no"),
                       mps=FALSE,
                       limmaVoom=FALSE,
                       appendGmt=NULL,
                       qos=c("short", "interactive", "normal"),
                       debug=FALSE) {
  qos <- match.arg(qos)
  overwrite <- match.arg(overwrite)
  ans <- NA
  if(overwrite=="ask") {
    if(dir.exists(outdir)) {
      msg <- sprintf("Directory %s exists. Overwritte(y/N)?[N]", outdir)
      while(!ans %in% c("N",  "y")) {
        if(!is.na(ans)) {
          message(sprintf("Invalid input %s", ans))
        }
        ans <- readline(msg)
        if(ans=="" || ans=="n") {
          ans <- "N"
        }
      }
    } else {
      ans <- "y"
    }
  } else if (overwrite=="no") {
    ans <- "N"
  } else if (overwrite=="yes") {
    ans <- "y"
  }

  doOverwrite <- switch(ans,
                        "N"=FALSE,
                        "y"=TRUE)
  if(!doOverwrite & dir.exists(outdir)) {
    return(invisible(NULL))
  }

  comm <- slurmEdgeRcommand(dgeList=dgeList,
                            designMatrix=designMatrix,
                            contrastMatrix=contrastMatrix,
                            outdir=outdir,
                            outfilePrefix=outfilePrefix,
                            mps=mps,
                            limmaVoom=limmaVoom,
                            appendGmt=appendGmt,
                            qos=qos,
                            debug=debug)
  res <- system(comm, intern=TRUE)
  return(list(command=comm, output=res))
}

