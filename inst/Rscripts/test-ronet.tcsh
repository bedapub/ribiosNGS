set PDIR=/SOFT/bi/apps/ribios/ribiosNetwork/inst

## not supported format: gml as fallback
time $PDIR/Rscripts/ronet.Rscript -infile $PDIR/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet.abc
## guessing: gml
time $PDIR/Rscripts/ronet.Rscript -infile $PDIR/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet-guess

time $PDIR/Rscripts/ronet.Rscript -infile $PDIR/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet.gml
time $PDIR/Rscripts/ronet.Rscript -infile $PDIR/extdata/test-ronet-geneids.txt -maxConnector 1 -outfile /tmp/test-ronet-max1.gml
time $PDIR/Rscripts/ronet.Rscript -infile $PDIR/extdata/test-ronet-geneids.txt -maxConnector 5 -outfile /tmp/test-ronet-max5.gml