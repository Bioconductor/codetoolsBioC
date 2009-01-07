getNamespaceFileImports <- function(package, lib.loc = base:::.libPaths()) {
    importCategories <-  c("importClasses", "importMethods", "imports")
    fileContents <-
      parseNamespaceFile(package = package, package.lib = lib.loc,
                         mustExist = FALSE)[importCategories]
    names(fileContents) <- c("S4Classes", "S4Methods", "general")

    fileContents <-
      lapply(fileContents,
             function(x) {
                 structure(lapply(seq_len(length(x)),
                                  function(i) {
                                      if (length(x[[i]]) == 1L)
                                          ls(getNamespace(x[[i]]), all = TRUE)
                                      else
                                          sort(x[[i]][[2L]])
                                  }),
                                  names =
                                  unlist(lapply(x, "[[", 1L)))
             })

    if (all(unlist(lapply(fileContents, length)) == 0)) {
        funsAndVars <- list("functions" = list(), "variables" = list())
    } else {
        funsAndVars <-
          lapply(structure(seq_len(length(fileContents[["general"]])),
                           names = names(fileContents[["general"]])),
                 function(i) {
                     x <- fileContents[["general"]]
                     isFunction <-
                       unlist(lapply(x[[i]],
                                     function(y)
                                     tryCatch(is.function(get(y,
                                                  getNamespace(names(x)[i]))),
                                              error = function(e) FALSE)))
                     list("functions" = x[[i]][isFunction],
                          "variables" = x[[i]][!isFunction])
                 })
        funsAndVars <-
          list("functions" = lapply(funsAndVars, "[[", "functions"),
               "variables" = lapply(funsAndVars, "[[", "variables"))
    }

    lapply(c(fileContents[c("S4Classes", "S4Methods")], funsAndVars),
           function(x) {
               y <- x[unlist(lapply(x, length)) > 0]
               if (length(y) == 0)
                   list()
               else
                   y[order(names(y))]
           })
}
