library(ribiosBatchJobs)
library(ribiosUtils)

f <- function(x) {
  if (x > 4) Sys.sleep(x/50)
  if (x == 7) stop("Invalid 'x': ", x)
  -x
}

loadConfigBioinfo <- function() {
    loadConfig(system.file("templates",
                           "BatchJobs-bioinfo.R",
                           package="ribiosBatchJobs"))
}

loadConfigBioinfo()

reg <- makeRegistry(id="HBcalcBioinfo",
                    file.dir=ribiosTempdir())
print(reg)
ids <- batchMap(reg, fun=f, 5:14)
print(ids)

## show status of registry
print(reg)
showStatus(reg)

done <- submitJobs(reg, resources=list(walltime=3600))

print(done)
showStatus(reg)

Sys.sleep(3)

## load result of job #2
loadResult(reg, 2)

## laod results of all 'done jobs'
loadResults(reg)

expRes <- list("1"=-5, "2"=-6, "4"=-8,
               "5"=-9, "6"=-10, "7"=-11,
               "8"=-12, "9"=-13, "10"=-14)
test_that("Results returned are correct", 
          expect_equal(expRes, loadResults(reg)))

removeRegistry(reg, ask="no")
