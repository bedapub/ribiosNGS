read_exprs_matrix <- function(x) {
  x <- path.expand(x)
  if(!file.exists(x))
    stop(paste(x, "does not exist\n"))
  x.con <- readLines(con=x, n=3L, warn=FALSE)
  if(grepl("^\\#1\\.2", x.con[1L])) { ## gct format
    mat <- .Call("read_gct", x, NULL, keep.desc=TRUE);
    return(mat)
  } else {
    if (any(grepl("\t", x.con))) { ## tab-delimited file
      df <- read.table(x, sep="\t", row.names=NULL, header=TRUE, check.names=FALSE, comment.char="")
    } else { ## space-delimited file
      df <- read.table(x, sep="", row.names=NULL, header=TRUE, check.names=FALSE, comment.char="")
    }
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
