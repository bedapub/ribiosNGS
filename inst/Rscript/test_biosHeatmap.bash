#!/bin/bash

./biosHeatmap.Rscript -infile ../extdata/biosHeatmap_testdata.txt -outfile ../extdata/biosHeatmap_testdata.pdf
./biosHeatmap.Rscript -infile ../extdata/biosHeatmap_testdata_tab.txt -outfile ../extdata/biosHeatmap_testdata_tab.pdf
./biosHeatmap.Rscript -infile ../extdata/biosHeatmap_testdata.gct -outfile ../extdata/biosHeatmap_testdata_gct.pdf

rm ../extdata/biosHeatmap_*.pdf
