library(ribiosAnnotation)

stopifnot(identical(bioc2gti("hgu133plus2"),
                    "HG-U133_PLUS_2"))

stopifnot(identical(bioc2gti(c("hgu133plus2", "hgu95av2", "bad_array")),
                    c("HG-U133_PLUS_2", "HG_U95AV2", NA)))
stopifnot(identical(gti2bioc("HG_U95AV2"),
                    "hgu95av2"))
stopifnot(identical(gti2bioc(c("HG_U95AV2", "CANINE", "HG_U95A")),
                    c("hgu95av2", "canine", "hgu95a")))
