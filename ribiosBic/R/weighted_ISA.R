## weightedISA: ISA for differentially regulated clusters
getBinaryWeight <- function(factor) {
  factorLength <- length(factor)
  factorChar <- as.character(factor)

  factor.expand <- expand.grid(factor, factor)
  factor.value <- as.numeric(factor.expand[,1]!=factor.expand[,2])
  factor.mat <- matrix(factor.value,nrow=factorLength, ncol=factorLength,
                       dimnames=list(factorChar, factorChar))
  return(factor.mat)
}
isa.weight.step <- function (normed.data, rows, thr.row, thr.col, direction,
                             sample.weight.matrix) {
    Ec <- normed.data$Ec
    Er <- normed.data$Er
    direction <- rep(direction, length = 2)
    
    if (any(!direction %in% c("up", "updown", "down"))) {
      stop("Invalid `direction' argument")
    }
    if (dim(sample.weight.matrix)[1] != ncol(Ec) || dim(sample.weight.matrix)[1] != dim(sample.weight.matrix)[2]) {
      stop("Weight matrix must be a square matrix with both dimension equal to the level number of group")
    }
    
    filter <- function(x, t, dir) {
        if (dir == "updown") {
            x <- .Call("beta_filter_updown_vart", x, as.double(t), 
                PACKAGE = "isa2")
        }
        else if (dir == "up") {
            x <- .Call("beta_filter_up_vart", x, as.double(t), 
                PACKAGE = "isa2")
        }
        else {
            x <- .Call("beta_filter_down_vart", x, as.double(t), 
                PACKAGE = "isa2")
        }
    }
    filter.weight <- function(x, t, weight, dir) {
      if (dir != "updown") {
        stop("'filter.weight' has only been implemented for 'updown'!")
      }
      x <- .Call("beta_weight_filter_updown_vart",
                 x, as.double(t), weight,
                 PACKAGE = "ribios")
    }

    ## browser();
    ## only the column-wise group relationships are considered now
    if ("hasNA" %in% names(attributes(normed.data)) && !attr(normed.data, 
                                                             "hasNA")) {
      col.new <- filter.weight(Er %*% rows, thr.col, sample.weight.matrix, direction[1]) ## sample x seed
      row.new <- filter(Ec %*% col.new, thr.row, direction[2]) ## gene x seed
    } else {
      col.new <- filter.weight(na.multiply(Er, rows), thr.col, direction[1])
      row.new <- filter(na.multiply(Ec, col.new), thr.row, 
                        direction[2])
    }
    list(columns = col.new, rows = row.new)
}


isa.weight.iterate <- function (normed.data,
                              group,
                              row.seeds,
                              col.seeds,
                              thr.row,
                              thr.col = thr.row, 
                              direction = c("updown", "updown"),
                              convergence = c("corx", "cor", "eps"),
                              cor.limit = 0.99, eps = 1e-04, corx = 3,
                              oscillation = FALSE, maxiter = 100) {
  isa2:::isa.status("Doing ISA iteration", "in")
  if ((missing(row.seeds) && missing(col.seeds))) {
    stop("No seeds, nothing to do")
  }
  if (!missing(row.seeds) && nrow(row.seeds) != ncol(normed.data$Er)) {
      stop("Invalid row seed length")
    }
  if (!missing(col.seeds) && nrow(col.seeds) != ncol(normed.data$Ec)) {
    stop("Invalid column seed length")
  }
  if (thr.row < 0 || thr.col < 0) {
    warning("Negative thresholds, are you sure about this?")
  }
  direction <- rep(direction, length = 2)
  if (any(!direction %in% c("up", "down", "updown"))) {
    stop("Invalid `direction' argument, should be `down', `up' or `updown'.")
  }
  convergence <- match.arg(convergence)
  if (convergence == "cor") {
    if (cor.limit <= 0) {
      warning("Non-positive correlation limit for convergence.")
    }
    }
  if (convergence == "eps") {
    if (eps >= 1) {
      warning("`eps' limit for convergence greater than one.")
    }
  }
  no.seeds <- 0
  if (!missing(row.seeds)) {
    no.seeds <- no.seeds + ncol(row.seeds)
  }
  if (!missing(col.seeds)) {
    no.seeds <- no.seeds + ncol(col.seeds)
  }
  orig.tg <- thr.row
  orig.tc <- thr.col
  if (length(thr.row) != 1 && length(thr.row) != no.seeds) {
    stop("`thr.row' does not have the right length")
  }
  if (length(thr.col) != 1 && length(thr.col) != no.seeds) {
    stop("`thr.col' does not have the right length")
  }
  thr.row <- rep(thr.row, length = no.seeds)
  thr.col <- rep(thr.col, length = no.seeds)
  all.seeds <- matrix(ncol = 0, nrow = nrow(normed.data$Ec))
  if (!missing(row.seeds)) {
    all.seeds <- cbind(all.seeds, row.seeds)
  }
  if (!missing(col.seeds)) {
    col.seeds <- isa2:::isa.row.from.col(normed.data,
                                         col.seeds = col.seeds, 
                                         thr.row = tail(thr.row, ncol(col.seeds)),
                                         direction = direction[2])
    all.seeds <- cbind(all.seeds, col.seeds)
  }
  rundata <- list(direction = direction, eps = eps, cor.limit = cor.limit, 
                  maxiter = maxiter, N = no.seeds, convergence = convergence, 
                  prenormalize = attr(normed.data, "prenormalize"),
                  hasNA = attr(normed.data, "hasNA"),
                  corx = corx, unique = FALSE, oscillation = oscillation)
  seeddata <- data.frame(iterations = NA, oscillation = 0, 
                         thr.row = thr.row, thr.col = thr.col, freq = rep(1, no.seeds), 
                         rob = rep(NA, no.seeds))
  if (length(all.seeds) == 0) {
    return(list(rows = all.seeds, columns = matrix(ncol = 0, 
                                    nrow = ncol(normed.data$Ec)), rundata = rundata, 
                seeddata = seeddata))
  }
  if (convergence == "eps") {
    check.convergence <- function(row.old, row.new, col.old, 
                                  col.new) {
      res <- (apply(row.old - row.new, 2,
                    function(x) all(abs(x) < eps)) &
              apply(col.old - col.new, 2,
                    function(x) all(abs(x) < eps)))
      res & !is.na(res)
    }
  }  else if (convergence == "cor") {
    check.convergence <- function(row.old, row.new, col.old, 
                                  col.new) {
      g.o <- scale(row.old)
      g.n <- scale(row.new)
      c.o <- scale(col.old)
      c.n <- scale(col.new)
      res <- (colSums(g.o * g.n)/(nrow(g.o) - 1) > cor.limit & 
              colSums(c.o * c.n)/(nrow(c.o) - 1) > cor.limit)
      res & !is.na(res)
    }
  } else if (convergence == "corx") {
      if (corx < 2) {
        stop("Invalid `corx' value, shoudl be at least 2")
      }
      rows.old <- cols.old <- list()
      idx.old <- seq_len(corx)
      check.convergence <- function(row.old, row.new, col.old, 
                                    col.new) {
        if (iter < corx + 1) {
          rows.old <<- c(rows.old, list(row.new))
          cols.old <<- c(cols.old, list(col.new))
          return(rep(FALSE, ncol(row.old)))
        }
        row.new <- scale(row.new)
        col.new <- scale(col.new)
        res <- (colSums(rows.old[[idx.old[1]]] * row.new)/(nrow(row.new) - 
                                                           1) > cor.limit & colSums(cols.old[[idx.old[1]]] * 
                                                                                    col.new)/(nrow(col.new) - 1) > cor.limit)
        rows.old[[idx.old[1]]] <<- row.new
        cols.old[[idx.old[1]]] <<- col.new
        idx.old <<- c(idx.old[-1], idx.old[1])
        res & !is.na(res)
      }
    }
  iter <- 0
  index <- seq_len(ncol(all.seeds))
  if (oscillation) {
    fire <- character(no.seeds)
  }
  row.old <- all.seeds
  col.old <- matrix(NA, nrow = ncol(normed.data$Ec), ncol = no.seeds)
  row.res <- matrix(NA, nrow = nrow(normed.data$Ec), ncol = no.seeds)
  col.res <- matrix(NA, nrow = ncol(normed.data$Ec), ncol = no.seeds)

  group.matrix <- getBinaryWeight(group)
  
  ## from here the step is performed in C
  ## TODO: Check how to incooperate a weight
  ## browser()
  while (TRUE) {
    iter <- iter + 1
    one.step <- isa.weight.step(normed.data, rows = row.old,
                                thr.row = thr.row, thr.col = thr.col,
                                direction = direction,
                                sample.weight.matrix=group.matrix)
    ## browser()
    row.new <- one.step$rows
    col.new <- one.step$columns
    conv <- check.convergence(row.old = row.old, row.new = row.new, 
                              col.old = col.old, col.new = col.new)
    zero <- apply(row.new, 2, function(x) all(x == 0))
    if (oscillation && iter > 1) {
      new.fire <- apply(row.new, 2, function(x) sum(round(x, 
                                                          4)))
      fire <- paste(sep = ":", fire, new.fire)
      osc <- logical(ncol(row.new))
      osc[(mat <- regexpr("(:.*:.*)\\1$", fire)) != -1] <- TRUE
      osc <- osc & !conv
      if (any(osc)) {
        mat <- cbind(mat[osc], attr(mat, "match.length")[[1]][osc])
        mat <- sapply(seq(length = nrow(mat)), function(x) substr(fire[osc][x], 
                            mat[x, 1], mat[x, 1] + mat[x, 2]))
        mat <- sapply(mat, function(x) sum(utf8ToInt(x) == 
                                           58), USE.NAMES = FALSE)
        seeddata$oscillation[index[osc]] <- mat/2
      }
    }
    else {
      osc <- FALSE
    }
    drop <- which(conv | zero | osc)
    if (length(drop) != 0) {
      row.res[, index[drop]] <- row.new[, drop]
      col.res[, index[drop]] <- col.new[, drop]
      seeddata$iterations[index[drop]] <- iter
      row.new <- row.new[, -drop, drop = FALSE]
      col.new <- col.new[, -drop, drop = FALSE]
      if (oscillation) {
        fire <- fire[-drop]
      }
      thr.row <- thr.row[-drop]
      thr.col <- thr.col[-drop]
      if (convergence == "corx") {
        rows.old <- lapply(rows.old, function(x) x[, 
                                                   -drop, drop = FALSE])
        cols.old <- lapply(cols.old, function(x) x[, 
                                                   -drop, drop = FALSE])
            }
      index <- index[-drop]
    }
    if (ncol(row.new) == 0 || iter > maxiter) {
      break
    }
    row.old <- row.new
    col.old <- col.new
  }
  isa2:::isa.status("DONE", "out")
  list(rows = row.res, columns = col.res, rundata = rundata, 
       seeddata = seeddata)
}


isa.weight <- function(data,
                     group,
                     thr.row = seq(1, 3, by = 0.5),
                     thr.col = seq(1, 3, by = 0.5),
                     no.seeds = 100, direction = c("updown", "updown")
                     ) {
  isa2:::isa.status("Performing complete ISA work flow", "in")
  if (!is.matrix(data)) {
    stop("`data must be a matrix")
  }
  normed.data <- isa.normalize(data)
  ##  row.seeds.sparsity <- rep(c(1,2,5,15, 25), length=no.seeds)
  row.seeds <- generate.seeds(length = nrow(data),
                              count = no.seeds)
  thr.list <- expand.grid(thr.row = thr.row, thr.col = thr.col)
  thr.list <- unlist(apply(thr.list, 1, list), rec = FALSE)
  isaresults <- lapply(thr.list, function(x) isa.weight.iterate(normed.data,
                                                              group,
                                                              row.seeds = row.seeds, thr.row = x["thr.row"], thr.col = x["thr.col"], 
                                                              direction = direction))
  isaresults <- lapply(isaresults, function(x) isa.unique(normed.data, 
                                                          x))
  isaresults <- lapply(isaresults, function(x) isa.filter.robust(data = data, 
                                                                 normed.data = normed.data, isares = x, row.seeds = row.seeds))
  result <- list()
  result$rows <- do.call(cbind, lapply(isaresults, "[[", "rows"))
  result$columns <- do.call(cbind, lapply(isaresults, "[[", 
                                          "columns"))
  result$seeddata <- do.call(rbind, lapply(isaresults, "[[", 
                                           "seeddata"))
  result$rundata <- isaresults[[1]]$rundata
  result$rundata$N <- sum(sapply(isaresults, function(x) x$rundata$N))
  result <- isa.unique(normed.data, result)
  isa2:::isa.status("DONE", "out")
  result
}

weightedISA <- function (data, flist = filterfun(function(x) IQR(x) > 0.5), 
                  group,
                  uniqueEntrez = TRUE,
                  thr.gene = seq(2, 4, by = 0.5),
                  thr.cond = seq(1, 3, by = 0.5), no.seeds = 100) {
  stopifnot(length(group) == dim(data)[2])
  if(!is.factor(group))
    group <- factor(group)
  
  isa2:::isa.status("ISA on an ExpressionSet", "in")
  if (!is(data, "ExpressionSet")) {
    stop("Please supply an ExpressionSet object")
  }
  if (is.function(flist)) {
    library(genefilter)
    selected <- genefilter(data, flist)
    data <- data[selected, ]
  }
  else if (!is.na(flist)) {
    stop("Could not interpret `flist' argument, should be a function or `NA'")
  }
  if (uniqueEntrez) {
    library(paste(sep = "", annotation(data), ".db"), character.only = TRUE)
    ENTREZ <- get(paste(sep = "", annotation(data), "ENTREZID"))
    entrez <- mget(featureNames(data), ENTREZ)
    keep <- sapply(entrez, function(x) length(x) > 1 || !is.na(x))
    data <- data[keep, ]
    vari <- apply(exprs(data), 1, var)
    larg <- findLargest(featureNames(data), vari, data = annotation(data))
    data <- data[larg, ]
  }
  modules <- isa.weight(exprs(data),
                      group=group,
                      thr.row = thr.gene,
                      thr.col = thr.cond, 
                      no.seeds = no.seeds)
  modules <- eisa:::isa.result.to.ISAModules(modules, data)
  isa2:::isa.status("DONE", "out")
  modules
}
