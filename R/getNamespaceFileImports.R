### FIXME: This is *not* exported and nowhere used  -- ???
getNamespaceFileImports <- function(package, lib.loc = .libPaths()) {
    importCategories <-  c("importClasses", "importMethods", "imports")
    fileContents <-
      parseNamespaceFile(package = package, package.lib = lib.loc,
                         mustExist = FALSE)[importCategories]
    names(fileContents) <- c("S4Classes", "S4Methods", "general")

    fileContents <- lapply(fileContents, function(x) {
        vals <- lapply(x, function(xi) {
            if (length(xi) == 1L)
                ls(getNamespace(xi), all.names = TRUE)
            else
                sort(xi[[2L]])
        })
        setNames(vals, vapply(x, "[[", "", 1L))
    })

    if (all(vapply(fileContents, length, 1L) == 0L)) {
        funsAndVars <- list("functions" = list(), "variables" = list())
    } else {
        fgen <- fileContents[["general"]]
        nms.fgen <- names(fgen)
	X <- setNames(seq_along(fgen), nms.fgen)
        funsAndVars <- lapply(X, function(i) {
            x.i <- fgen[[i]]
            isFunction <- vapply(x.i, function(y)
                tryCatch(is.function(get(y, getNamespace(nms.fgen[i]))),
                         error = function(e) FALSE), NA)
            list("functions" = x.i[isFunction],
                 "variables" = x.i[!isFunction])
        })
        funsAndVars <-
          list("functions" = lapply(funsAndVars, "[[", "functions"),
               "variables" = lapply(funsAndVars, "[[", "variables"))
    }

    X <- c(fileContents[c("S4Classes", "S4Methods")], funsAndVars)
    lapply(X, function(x) {
        y <- x[vapply(x, length, 1L) > 0L]
        if (length(y) == 0L) list() else y[order(names(y))]
    })
}
