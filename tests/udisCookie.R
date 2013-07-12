library(ribiosUDIS)

##cookie <- bakeCookie(appname="udis_expression", user="wangy78", apppassword="klee")
##exampleURL <- "http://udisdev.roche.com/udiscgiqa/expressionData_cgi?query=signals&studyidexternal=NCS_tissue_rat&studydomain=undefined&format=gct&outdest=browser"
##myQuery <- queryUrl(exampleURL, cookie)

system.time(test <- getUDISexpression(id="GSE20986"))
stopifnot(is(test, "ExpressionSet"))

myIden <- function(x,y) {
  stopifnot(identical(exprs(x), exprs(y)))
  stopifnot(identical(fData(x), fData(y)))
  stopifnot(identical(pData(x), pData(y)))
}

system.time(myEset1_1 <- getUDISexpression(id="GSE20986"))
system.time(myEset1_2 <- getUDISexpression(id="GSE20986", idType="studyIdExternal"))
myIden(myEset1_1, myEset1_2)

system.time(myEset2_1 <- getUDISexpression(id=1211, idType="datasetId"))
system.time(myEset2_2 <- getUDISexpression(id=3084, idType="studyId"))
system.time(myEset2_3 <- getUDISexpression(id="UUO_Mouse_Chugai", idType="studyTitle"))
system.time(myEset2_4 <- getUDISexpression(id="UUO_Mouse_Chugai", idType="studyIdExternal"))
myIden(myEset2_1, myEset2_2)
myIden(myEset2_1, myEset2_3)
myIden(myEset2_1, myEset2_4)

myEset3_1 <- getUDISexpression(id=3220, idType="studyId",probeproptype="HUMANEG_ID", probeprop=7350)
