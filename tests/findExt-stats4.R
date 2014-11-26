library(codetoolsBioC)
## This is not a too good long-time strict test,
## as all these packages will change...

## OTOH, for now, it is important to have *some* strict test.
options(width=75)

library(methods)
(ed.meth <- findExternalDeps("methods"))
f.meth <- ed.meth$functions
stopifnot(identical(list(), ed.meth$ S4Classes),
	  identical(list(), ed.meth$ S4Methods),
	  c("base","stats","utils") %in% names(f.meth),
	  "as.ts" %in% f.meth$stats,
	  length(f.meth$base) == 279,
	  c("find", "prompt") %in% f.meth$utils)

library(stats4)
(ed.s4 <- findExternalDeps("stats4"))
stopifnot(
    c("character", "function", "integer", "language",
      "list", "matrix", "missing", "numeric") %in% ed.s4$S4Classes$methods)


(ed.tools <- findExternalDeps("tools"))

(ed.Matr <- findExternalDeps("Matrix"))
