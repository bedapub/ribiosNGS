#! /bin/bash

## Install the current R package to all R versions in use, including the latest version as well as the version used in RStudio
ml R/3.3.2-goolf-1.7.20
make install
ml load R/3.4.0-goolf-1.7.20
make install
ml load R/3.5.1-goolf-1.7.20
make install

