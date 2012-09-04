getNamespaceFileImports <- function(package, lib.loc = base:::.libPaths()) {
    importCategories <-  c("importClasses", "importMethods", "imports")
    fileContents <-
      parseNamespaceFile(package = package, package.lib = lib.loc,
                         mustExist = FALSE)[importCategories]
    names(fileContents) <- c("S4Classes", "S4Methods", "general")

    fileContents <- lapply(fileContents, function(x) {
        vals <- lapply(seq_len(length(x)), function(i) {
            if (length(x[[i]]) == 1L)
                ls(getNamespace(x[[i]]), all = TRUE)
            else
                sort(x[[i]][[2L]])
        })
        setNames(vals, ulapply(x, "[[", 1L))
    })

    if (all(ulapply(fileContents, length) == 0)) {
        funsAndVars <- list("functions" = list(), "variables" = list())
    } else {
        X <- setName(seq_len(length(fileContents[["general"]])),
                     names(fileContents[["general"]]))
        funsAndVars <- lapply(X, function(i) {
            x <- fileContents[["general"]]
            isFunction <- ulapply(x[[i]], function(y) tryCatch({
                is.function(get(y, getNamespace(names(x)[i])))
            }, error = function(e) {
                FALSE
            }))
            list("functions" = x[[i]][isFunction],
                 "variables" = x[[i]][!isFunction])
        })
        funsAndVars <-
          list("functions" = lapply(funsAndVars, "[[", "functions"),
               "variables" = lapply(funsAndVars, "[[", "variables"))
    }

    X <- c(fileContents[c("S4Classes", "S4Methods")], funsAndVars)
    lapply(X, function(x) {
        y <- x[ulapply(x, length) > 0]
        if (length(y) == 0)
            list()
        else
            y[order(names(y))]
    })
}
