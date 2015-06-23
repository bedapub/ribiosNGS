gseaChiptypes <- function(pattern=".*",...) dir(GSEA_ANNOTATION_DIR, pattern=pattern,...)

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

  chip.file <- file.path(GSEA_ANNOTATION_DIR,
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
    system(gsea.command, intern=FALSE, ignore.stderr=FALSE, wait=TRUE)
  }
}


