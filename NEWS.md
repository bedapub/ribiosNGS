# Changes in version 0.99.0 (2026-02-14)

## Bioconductor submission

- Prepared package for Bioconductor submission.
- Set version to 0.99.0 as required for new Bioconductor submissions.
- Updated DESCRIPTION: expanded Description field, added `biocViews`,
  updated minimum R version to >= 4.4.0, added `VignetteBuilder`.
- Added `@return` documentation tags to all exported functions.
- Added introductory vignette with worked example.
- Converted `\dontrun{}` to `\donttest{}` where appropriate.
- Ensured `par()` settings are saved and restored via `on.exit()`.
- Added GitHub Actions CI workflow for R CMD check.

# Changes in version 1.5.1 (2026-01-30)

- Updated ribiosNGS to fit the latest biokit output structure.
- ggpubr is now only suggested, not imported.
- Updated SLURM integration to work with latest HPC updates.

# Changes in version 1.0-14 (2015-12-03)

- Added `truncatedDgeTable` and `writeTruncatedDgeTables`.

# Changes in version 1.0-13 (2015-08-11)

- Replace NA with zero counts if needed.

# Changes in version 1.0-12 (2015-06-12)

- RiboSeq: fixed a bug in matching the two matrices by row names.

# Changes in version 1.0-11 (2015-02-16)

- Substantial update of S4 classes and functions.
- Added `annotate` method.

# Changes in version 1.0-7 (2014-11-03)

- Added `EdgeResult` object.
- Exported functions needed for MPS.

# Changes in version 1.0-6 (2014-10-31)

- Started using roxygen2 for documentation.
- edgeR workflow wrapped into `edgeBuild`, `edgeTest`, and `edgeRun`.
- Removed `edge.DGE` function (no longer used).

# Changes in version 1.0-5 (2013-01-22)

- pairs.Rscript moved to /SOFT/bi/apps/ribios/scripts/pairs.

# Changes in version 1.0-4 (2012-09-10)

- Bug fix: `read.table` in merge.Rscript does not recognize single quote as
  quote character.

# Changes in version 1.0-3 (2012-06-19)

- Modified merge.Rscript: suffixes do not cause error, `-outfile` can be
  missing (stdout as default), `-noheader/-header` is a mandatory parameter.

# Changes in version 1.0-2 (2011-12-20)

- Added documentation and testing data for `edgeR.DGE`.
