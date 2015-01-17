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


## Imports via ::
globals <- findGlobalsBioC(function() base::cat())
print(globals)
## FIXME: findGlobalsBioC() fails to locate <pkg>::<object>
## stopifnot(all(is.element(c("cat"), globals)))
