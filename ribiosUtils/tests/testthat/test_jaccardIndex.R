expect_identical(ribiosUtils::jaccardIndex(LETTERS[1:5], LETTERS[3:8]),3/8)
expect_identical(ribiosUtils::jaccardIndex(LETTERS[1:2], LETTERS[3:8]),0)
expect_identical(ribiosUtils::jaccardIndex(LETTERS[1:4], LETTERS[1:4]),1)

expect_identical(ribiosUtils::jaccardDistance(LETTERS[1:5], LETTERS[3:8]),5/8)
expect_identical(ribiosUtils::jaccardDistance(LETTERS[1:2], LETTERS[3:8]),1)
expect_identical(ribiosUtils::jaccardDistance(LETTERS[1:4], LETTERS[1:4]),0)
