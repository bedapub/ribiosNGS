matchv <- function(x,table,...) table[match(x, table,...)]
imatch <- function(x, table,...) match(tolower(x), tolower(table),...)
imatchv <- function(x,table,...) table[imatch(x, table,...)]
ipmatch <- function(x, table,...) pmatch(tolower(x), tolower(table),...)
ipmatchv <- function(x,table,...) table[ipmatch(x, table,...)]
