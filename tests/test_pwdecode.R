library(ribiosUtils)
##dyn.load("/apps/bi/apps/ribios/ribiosUtils/src/endec.so")

test <- " \\001\\000\\141\\314\\033\\033\\033\\033\\033\\142\\303\\056\\166\\311\\037\\042"

stopifnot(identical("HSV", pwdecode(test)))

stopifnot(identical("abcs", pwdecode("abcs")))
