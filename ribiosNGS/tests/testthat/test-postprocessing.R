library(ribiosNGS)

expect_identical(ribiosNGS::parseContributingGenes("RXRA(-1.42),CD36(-1.21),PPARG(-0.56),NR1H3(-0.39),CEBPA(-0.28)")[[1]],
                 data.frame(Gene=c("RXRA", "CD36", "PPARG", "NR1H3", "CEBPA"),
                            Stat=c(-1.42, -1.21, -0.56, -0.39, -0.28)))

