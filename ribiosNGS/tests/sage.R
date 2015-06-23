library(ribiosNGS)

DATA_DIR <- "/data/bioseq04/minipig/transcriptome/TestisContigs/sage"
if(file.exists(DATA_DIR)) {
  d <- edgeR.DGE(path=DATA_DIR)
}
