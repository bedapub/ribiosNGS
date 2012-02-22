library(ribiosUDIS)

cookie <- bakeCookie(appname="udis_expression", user="wangy78", apppassword="klee")
exampleURL <- "http://udisdev.roche.com/udiscgiqa/expressionData_cgi?query=signals&studyidexternal=NCS_tissue_rat&studydomain=undefined&format=gct&outdest=browser"
##myQuery <- queryUrl(exampleURL, cookie)

system.time(test <- queryExpressionData(studyid="NCS_tissue_rat", cookie=cookie))
