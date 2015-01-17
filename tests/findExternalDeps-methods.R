library(codetoolsBioC)
## This is not a too good long-time strict test,
## as all these packages will change...

message("Package: methods")
library(methods)
ed.meth <- findExternalDeps("methods")
f.meth <- ed.meth$functions
str(f.meth)

stopifnot(identical(list(), ed.meth$S4Classes),
	  identical(list(), ed.meth$S4Methods))

desc <- packageDescription("methods")
imports <- unlist(strsplit(gsub(" ", "", desc$Imports), split=","))
stopifnot(c("base", imports) %in% names(f.meth))

stopifnot(length(f.meth$base) > 200,
	  c("find", "prompt") %in% f.meth$utils)
