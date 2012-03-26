set PDIR=/SOFT/bi/apps/ribios/ribiosNetwork/inst

time $PDIR/Rscripts/ronet.Rscript -infile $PDIR/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet.out
time $PDIR/Rscripts/ronet.Rscript -infile $PDIR/extdata/test-ronet-geneids.txt -maxConnector 1 -outfile /tmp/test-ronet-max1.out
time $PDIR/Rscripts/ronet.Rscript -infile $PDIR/extdata/test-ronet-geneids.txt -maxConnector 5 -outfile /tmp/test-ronet-max5.out