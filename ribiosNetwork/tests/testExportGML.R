library(ribiosNetwork)
g <- barabasi.game(100, directed=FALSE)
V(g)$label <- c(paste("node", 1:99, sep=""),"--")
V(g)$name <- 1:100
V(g)$isInput <- rbinom(100,1, 0.5)
E(g)$label <- "Expression"
gPosE <- as.logical(rbinom(ecount(g), 1, 0.25))
gNegE <- as.logical(rbinom(ecount(g), 1, 0.25))
E(g)$label[gPosE] <- "Expressoion_Positive"
E(g)$label[gNegE] <- "Expressoion_Negative"

gFile <- tempfile()
exportGML(g, gFile)

testStr2 <- sprintf("grep -c 'label \"\"' %s", gFile)
testRes2 <- system(testStr2, intern=TRUE)
if(testRes2 != "1")
  stop("Error: dash-only labels are not eliminated")
