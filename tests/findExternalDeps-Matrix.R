library(codetoolsBioC)
## This is not a too good long-time strict test,
## as all these packages will change...
## OTOH, for now, it is important to have *some* strict test.

ns0 <- loadedNamespaces() # Namespaces before

message("Package: Matrix")
ed.Matr <- findExternalDeps("Matrix")
str(ed.Matr)

## Cleanup such that the test scripts can be called repeatedly
unloadNamespace("Matrix")
unloadNamespace("lattice")
unloadNamespace("grid")

stopifnot(identical(loadedNamespaces(), ns0))
