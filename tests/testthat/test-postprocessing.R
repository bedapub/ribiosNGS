library(ribiosNGS)

expect_identical(ribiosUtils::parseContributingGenes("RXRA(-1.42),CD36(-1.21),PPARG(-0.56),NR1H3(-0.39),CEBPA(-0.28)"),
                 data.frame(Gene=c("RXRA", "CD36", "PPARG", "NR1H3", "CEBPA"),
                            Stat=c(-1.42, -1.21, -0.56, -0.39, -0.28)))

expect_identical(ribiosUtils::cameraScore(c(0.01, 0.1, 1), c("Up", "Down", "Up")),
                 c(2, -1, 0))




expect_identical(ribiosUtils::jaccardIndex(LETTERS[1:5], LETTERS[3:8]),3/8)
expect_identical(ribiosUtils::jaccardIndex(LETTERS[1:2], LETTERS[3:8]),0)
expect_identical(ribiosUtils::jaccardIndex(LETTERS[1:4], LETTERS[1:4]),1)

expect_identical(ribiosUtils::jaccardDist(LETTERS[1:5], LETTERS[3:8]),5/8)
expect_identical(ribiosUtils::jaccardDist(LETTERS[1:2], LETTERS[3:8]),1)
expect_identical(ribiosUtils::jaccardDist(LETTERS[1:4], LETTERS[1:4]),0)
