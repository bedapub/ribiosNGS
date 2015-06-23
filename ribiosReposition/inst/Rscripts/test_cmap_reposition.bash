./cmap_reposition.Rscript -uptags ../extdata/cmapExample_uptags.txt -downtags ../extdata/cmapExample_downtags.txt -outfile out.txt -verbose 1

cut -f 2,3,8 out.txt | head -n 10
