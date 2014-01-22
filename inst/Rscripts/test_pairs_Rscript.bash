#!/bin/bash

./pairs.Rscript -infile ../extdata/test_pairs.txt -outfile test_pairs.png -lm -cor && display test_pairs.png
./pairs.Rscript -infile ../extdata/test_pairs.txt -outfile test_pairs.tiff -lm -cor -log10 && display test_pairs.tiff
./pairs.Rscript -infile ../extdata/test_pairs_binary.txt -outfile test_pairs.pdf -lm -corr -log10 && display test_pairs.pdf
rm test_pairs.pdf test_pairs.png test_pairs.tiff
