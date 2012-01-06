BIOS_GSEA_ANNOTATION_DIR <- "/DATA/bi/httpd_8080/htdoc/apps/gsea/annotations"

biosGSEAannotations <- function(pattern=".*") {
  dir(BIOS_GSEA_ANNOTATION_DIR, pattern=pattern)
}

## An example of running GSEA on montale
##/SOFT/bi/apps/java/c/bin/java -Xmx2500m -classpath .:/SOFT/bi/apps/gsea/GSEA2-2.02/my_gsea2.jar xtools.gsea.GseaPreranked
##-gmx /DATA/bi/httpd_8080/htdoc/sawitmp/gsea_1927.gmt
##-chip /DATA/bi/httpd_8080/htdoc/apps/gsea/annotations/HG_U133_PLUS_2.chip
##-rnk /DATA/bi/httpd_8080/htdoc/sawitmp/TGGate_ranked_genelist_Acarbose.rnk
## -rpt_label runGSEA -nperm 1000 -collapse true
## -mode Max_probe -norm meandiv -include_only_symbols true -scoring_scheme weighted
## -make_sets true -plot_top_x 25 -rnd_seed timestamp -set_max 500 -set_min 5 -zip_report false -gui false -out /DATA/bi/httpd_8080/htdoc/sawitmp/GSEA_1928


biosGSEA <- function(files,
                     gmx="/DATA/bi3/RA_Ocrelizumab_WA20494_WA20497/RAnutley_GSEA_genesets.gmt",
                     chip="GENE_SYMBOL.chip",
                     nperm=1000L,
                     collapse=TRUE,
                     plotTopX=25,
                     outdir="./GSEA") {
  
  stopifnot(file.exists(gmx))
  if(!grepl("\\.chip$", chip))
    chip <- paste(chip, ".chip", sep="")

  chip.file <- file.path(BIOS_GSEA_ANNOTATION_DIR,
                         chip)
  stopifnot(file.exists(chip.file))
  
  gsea.command.template <- paste(c("/SOFT/bi/apps/java/c/bin/java -Xmx2500m",
                                   "-classpath .:/SOFT/bi/apps/gsea/GSEA2-2.02/my_gsea2.jar xtools.gsea.GseaPreranked",
                                   "-gmx %s",
                                   "-chip %s",
                                   "-rnk %s -rpt_label runGSEA -nperm %d -collapse %s",
                                   "-mode Max_probe -norm meandiv -include_only_symbols true",
                                   "-scoring_scheme weighted -make_sets true -plot_top_x %d",
                                   "-rnd_seed timestamp -set_min 5 -set_max 500", 
                                   "-zip_report false -gui false -out %s"),
                                 collapse=" ")
  
  for(i in seq(along=files)) {
    gsea.command <- sprintf(gsea.command.template,
                            gmx,
                            chip.file,
                            files[i],
                            as.integer(nperm),
                            ifelse(collapse, "true", "false"),
                            as.integer(plotTopX),
                            file.path(outdir,paste("GSEA", gsub("\\..*$", "", basename(files[i])), sep="_"))
                            )
    system(gsea.command, inter=FALSE, ignore.stderr=FALSE, wait=TRUE)
  }
}





## convert connectivity map score
ks <- function(vec, n) {
  vect <- length(vec)
  a <- max(sapply(seq(along=vec), function(x) x/vect - vec[x]/n))
  b <- max(sapply(seq(along=vec), function(x) vec[x]/n-(x-1)/vect))
  if(a>=b)
    return(a)
  else
    return(-b)
}

sScore <- function(rnk, up, down, perm=FALSE) {
  stopifnot(is.data.frame(rnk) & ncol(rnk)==2)
  colnames(rnk) <- c("gene", "score")
  if(perm) {
    up <- sample(rnk[,1], length(up), replace=TRUE)
    down <- sample(rnk[,1], length(down), replace=TRUE)
  }
  stopifnot(any(up %in% rnk$gene) & any(down %in% rnk$gene))
  rnk$gene <- as.character(rnk$gene)
  up <- as.character(up)
  down <- as.character(down)

  n <- nrow(rnk)
  tup <- length(up)
  tdown <- length(down)

  ## order gene list
  rnk <- rnk[order(rnk$score, decreasing=TRUE),]
  
  vup <- sort(match(up, rnk$gene), decreasing=FALSE)
  vdown <- sort(match(down, rnk$gene), decreasing=FALSE)

  ksup <- ks(vec=vup, n=n)
  ksdown <- ks(vec=vdown, n=n)

  if(sign(ksup)==sign(ksdown)) {
    cscore <- 0
  } else {
    cscore <- ksup - ksdown
  }

  return(c(cscore=cscore, ksup=ksup, ksdown=ksdown))
}

cmap <- function(rnks, up, down, perm=FALSE) {
  cscoresAll <- lapply(rnks, function(x) sScore(x, up=up, down=down, perm=perm))
  cscores <- sapply(cscoresAll, "[[", 1)
  ksups <- sapply(cscoresAll, "[[", 2)
  ksdowns <- sapply(cscoresAll, "[[", 3)
  p <- max(cscores)
  q <- min(cscores)
  S <- vector("numeric", length(rnks))
  S[cscores>0] <- cscores[cscores>0]/p
  S[cscores<0] <- -cscores[cscores<0]/q

  rind <- seq(along=rnks)
  ind <- rind[order(S, ksups, decreasing=TRUE)]

  ## permutation test
  
  res <- data.frame(index=ind,
                    cScore=S[ind],
                    raw.cScore=cscores[ind],
                    ksup=ksups[ind],
                    ksdown=ksdowns[ind])
  return(res)
}



