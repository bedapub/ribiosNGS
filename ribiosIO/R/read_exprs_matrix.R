#' Read an expression matrix from file
#'
#' @description
#' Read an expression matrix from file. The file is either a GCT format file, a tab-delimited file or a space-delimited file.
#'
#' @param x File name
#' @return A \code{matrix}
#' @details
#' An expression matrix of size m x n contains exprssion levels of m features in n samples. This function supports three commonly used file formats for expression levels: \code{GCT} format, \code{tab-delimited file} and \code{space-delimited file}.
#'
#' @author Jitao David Zhang <jitao_david.zhang at roche.com>
#' @note
#'   The function uses a very simple logic to guess whether the file is
#'  tab-delimited or space-delimited: it reads in the first n lines
#'  (currently \code{n=3}), and checks whether there is any tab character
#'  (\code{\\t}): if yes, the file is parsed as tab-delmited, otherwise as
#'  space-delimited. Therefore, a space-delimited file should not contain
#'  tabs in case it needs to be parsed.
#'
#'  From ribiosIO version 1.0.2, this function supports duplicated row
#'  names.
#'
#'  From ribiosIO version 1.0-21, this function supports matrix file in
#'  which the second column is not numeric. This can happen, for instance,
#'  if the user decides to include descriptions. If such descriptions are
#'  detected, they are stored in the attribute \dQuote{desc} so as to be
#'  later written into gct files.
#'
#'  From ribiosIO version 1.0-39, the function tolerates non-numeric values (such as '5?')
#'  in tab-delimited files better. However note that such values in the second column will cause problem because
#'  they will make the program interpret the second column as description but not numeric values.
#'
#' @seealso   The function calls internally the \code{read_gct_matrix} function to parse GCT files.
#' @examples
#' testfile.path <- system.file("extdata", package="ribiosIO")
#' 
#' ## import gct
#' read_exprs_matrix(file.path(testfile.path,"test_read_exprs_matrix.gct"))
#' 
#' ## import tab-separated file
#' read_exprs_matrix(file.path(testfile.path, "test_read_exprs_matrix.tsv"))
#' 
#' ## import space-separated file
#' read_exprs_matrix(file.path(testfile.path, "test_read_exprs_matrix.txt"))
#' 
#' ## import tab-separated file with descriptions
#' read_exprs_matrix(file.path(testfile.path, "test_read_exprs_matrix_desc.tsv"))
#'
#' ## import tab-separated file with non-numeric values
#' read_exprs_matrix(file.path(testfile.path, "test_nonnumbers.txt"))

read_exprs_matrix <- function(x) {
  x <- path.expand(x)
  if(!file.exists(x))
    stop(paste(x, "does not exist\n"))
  x.con <- readLines(con=x, n=3L, warn=FALSE)
  if(grepl("^\\#1\\.[2|3]", x.con[1L])) { ## gct format
      mat <- .Call("c_read_gct", x, NULL, keep.desc=TRUE);
      return(mat)
  } else {
      sep <- ifelse(any(grepl("\t", x.con)), "\t", "")
      df <- read.table(x, sep=sep, row.names=NULL, header=TRUE,
                       check.names=FALSE, stringsAsFactors=FALSE, comment.char="")      
      if(is.integer(df[,2L]) || is.numeric(df[,2L]))  { ## second column is numeric
          mat <- data.matrix(df[,-1L,drop=FALSE])
          colnames(mat) <- colnames(df)[c(-1L)]
          rownames(mat) <- df[,1L]
      } else { ## second column is not numeric
          mat <- data.matrix(df[, c(-1L, -2L),drop=FALSE])
          rownames(mat) <- df[,1L]
          colnames(mat) <- colnames(df)[c(-1L, -2L)]
          attr(mat, "desc") <- as.character(df[,2L])
      }
      return(mat)
  }
}
