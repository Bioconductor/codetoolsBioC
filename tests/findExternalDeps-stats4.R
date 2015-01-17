library(codetoolsBioC)
## This is not a too good long-time strict test,
## as all these packages will change...
## OTOH, for now, it is important to have *some* strict test.

ns0 <- loadedNamespaces() # Namespaces before

message("Package: stats4")
library(stats4)
ed.s4 <- findExternalDeps("stats4")
str(ed.s4)
stopifnot(
    c("character", "function", "integer", "language",
      "list", "matrix", "missing", "numeric") %in% ed.s4$S4Classes$methods)

# Cleanup such that the test scripts can be called repeatedly
unloadNamespace("stats4")
stopifnot(identical(loadedNamespaces(), ns0))
