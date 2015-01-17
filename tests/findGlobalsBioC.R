library(codetoolsBioC)
findGlobalsBioC <- codetoolsBioC:::findGlobalsBioC

globals <- findGlobalsBioC(function() cat())
print(globals)
stopifnot(all(is.element(c("cat"), globals)))

globals <- findGlobalsBioC(function() { cat() })
print(globals)
stopifnot(all(is.element(c("{", "cat"), globals)))

globals <- findGlobalsBioC(function() { cat(); print(1) } )
print(globals)
stopifnot(all(is.element(c("{", "cat", "print"), globals)))


## Imports via :: and :::
globals <- findGlobalsBioC(function() base::cat(), merge=FALSE)
str(globals)
stopifnot(all(is.element(c("base::cat"), unlist(globals))))

globals <- findGlobalsBioC(function() base:::cat(), merge=FALSE)
str(globals)
stopifnot(all(is.element(c("base:::cat"), unlist(globals))))


## Real-world example (also uses ::)
globals <- findGlobalsBioC(stats::Gamma, merge=FALSE)
str(globals)
