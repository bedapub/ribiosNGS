library(ribiosUtils)

expect_identical(ribiosUtils:::shortenStr("ETV"), "ETV")
expect_identical(ribiosUtils:::shortenStr("abcdefgh"), "abcdefgh")
expect_identical(ribiosUtils:::shortenStr("abcdefghijkl"), "abcdefgh...")

expect_identical(ribiosUtils:::fixWidthStr("ETV"), "ETV     ")
expect_identical(ribiosUtils:::fixWidthStr("abcdefgh"), "abcdefgh")
expect_identical(ribiosUtils:::fixWidthStr("abcdefghijkl"),
                 "abcde...")
