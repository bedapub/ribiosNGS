##----------------------------------------##
## generic method connScore
## ? overhead : S4 method does not have much more overhead than S3 function
##----------------------------------------##
setGeneric("connScore", function(x, up, down) standardGeneric("connScore"))


setMethod("connScore",
          c("character", "character", "character"),
          function(x,up,down) {
            n <- length(x)
            vup <- match(up, x)
            vdown <- match(down,x)
            connScore(x=n, up=vup, down=vdown)
          })
setMethod("connScore",
          c("numeric", "numeric", "numeric"),
          function(x,up,down) {
            if(length(x)!=1) {
              warning("'x' length larger than 1: it was treated as a character vector\n")
              return(connScore(as.character(x), as.character(up), as.character(down)))
            }
            if(all(is.na(up)) || all(is.na(down)))
              return(c(cscore=0, ksup=NA, ksdown=NA))
            ksup <- ksScore(n=x, vec=up)
            ksdown <- ksScore(n=x, vec=down)
            if(sign(ksup)==sign(ksdown)) {
              return(c(cscore=0, ksup=ksup, ksdown=ksdown))
            } else {
              return(c(cscore=ksup - ksdown, ksup=ksup, ksdown=ksdown))
            }
          })
setMethod("connScore",
          c("list", "character", "character"),
          function(x, up, down) {
            t(sapply(x, function(v) connScore(v, up, down)))
          })
setMethod("connScore",
          c("matrix", "numeric", "numeric"),
          function(x, up, down) {
            nr <- nrow(x)
            t(apply(x, 2L, function(v) connScore(nr, v[up], v[down])))
          })
setMethod("connScore",
          c("matrix", "character", "character"),
          function(x, up, down) {
            up.ind <- match(up, rownames(x))
            down.ind <- match(down, rownames(x))
            connScore(x, up=up.ind, down=down.ind)
          })

##----------------------------------------##
## connScorePerm
##----------------------------------------##
setGeneric("connScorePerm", function(x, up, down, B) standardGeneric("connScorePerm"))

setMethod("connScorePerm",
          c("numeric", "numeric", "numeric", "numeric"),
          function(x, up, down, B) {
            if(length(x)>1) {
              warning("'x' is a numeric vector: its length will be used for permutation.\n  To avoid this message, use length(x) as input.")
              x <- length(x)
            }
            if(length(up)>1) {
              warning("'up' is a numeric vector: its length will be used for permutation.\n  To avoid this message, use length(x) as input.")
              up <- length(up)
            }
            if(length(down)>1) {
              warning("'down' is a numeric vector: its length will be used for permutation.\n  To avoid this message, use length(x) as input.")
              down <- length(down)
            }
            stopifnot(length(x)==1 && length(up)==1 && length(down)==1)
            vec <- 1:x
            t(sapply(1:B, function(b) {
              vup <- sample(vec, up, replace=TRUE)
              vdown <- sample(vec, down, replace=TRUE)
              connScore(x=x, up=vup, down=vdown)
            }))
          })
setMethod("connScorePerm",
          c("matrix", "numeric", "numeric","numeric"),
          function(x, up, down, B) {
            connScorePerm(nrow(x), up=up, down=down, B=B)
          })
          
setMethod("connScorePerm",
          c("character", "numeric", "numeric", "numeric"),
          function(x, up, down, B) {
            connScorePerm(length(x), up=up, down=down, B=B)
          })
setMethod("connScorePerm",
          c("character", "character", "character", "numeric"),
          function(x, up, down, B) {
            connScorePerm(length(x), up=length(up), down=length(down), B=B)
          })
setMethod("connScorePerm",
          c("list", "numeric", "numeric", "numeric"),
          function(x,up, down, B) {
            lapply(x, function(t) connScorePerm(length(t), up=up, down=down, B=B))
          })
setMethod("connScorePerm",
          c("list", "character", "character", "numeric"),
          function(x,up, down, B) {
            nup <- length(up)
            ndown <- length(down)
            lapply(x, function(t) connScorePerm(length(t), up=nup, down=ndown, B=B))
          })
          
