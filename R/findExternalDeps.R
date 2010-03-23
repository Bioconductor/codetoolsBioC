findExternalDeps <- function(package) {
    getPackageEnvironment <- function(package) {
        pname <- paste("package", package, sep = ":")
        if (! pname %in% search())
            stop(package, " must be loaded")
        if (package %in% loadedNamespaces())
            getNamespace(package)
        else
            as.environment(pname)
    }

    packageEnv <- getPackageEnvironment(package)
    packageObjs <- getPackageTable(package)
    packageObjs <-
      packageObjs[packageObjs[["Origin"]] == package |
                  packageObjs[["Type"]] == "S4MethodsTable", , drop = FALSE]

    ## Cast wide net to find classes, functions, and variables
    packageClasses <- character(0L)
    packageGlobalsFunctions <- character(0L)
    packageGlobalsVariables <- character(0L)
    if (nrow(packageObjs) > 0L) {
        if (any(packageObjs[["Type"]] == "S4Class")) {
            packageClasses <-
              sort(unique(c(packageClasses,
                unlist(lapply(packageObjs[["Name"]][packageObjs[["Type"]] == "S4Class"],
                              function(x) {
                                  classDef <- get(x, packageEnv)
                                  containedClasses <- slot(classDef, "contains")
                                  c(unlist(lapply(slot(classDef, "slots"), unname)),
                                    names(containedClasses[
                                      unlist(lapply(containedClasses, function(y)
                                                    slot(y, "distance") == 1L))]))
                              })))))
        }
        if (length(ls(get(".__S3MethodsTable__.", packageEnv), all = TRUE)) > 0L) {
            packageGlobals <- 
              lapply(ls(get(".__S3MethodsTable__.", packageEnv), all = TRUE),
                     function(x)
                     findGlobalsBioC(get(x, get(".__S3MethodsTable__.", packageEnv)),
                                     merge = FALSE))
            packageGlobalsFunctions <-
              sort(unique(c(packageGlobalsFunctions,
                            unlist(lapply(packageGlobals, "[[", "functions")))))
            packageGlobalsVariables <-
              sort(unique(c(packageGlobalsVariables,
                            unlist(lapply(packageGlobals, "[[", "variables")))))
        }
        if (any(packageObjs[["Type"]] == "S4MethodsTable")) {
            externalMethods <-
              packageObjs[["Name"]][packageObjs[["Origin"]] != package &
                                    packageObjs[["Type"]] == "S4MethodsTable"]
            packageGlobalsFunctions <-
              sort(unique(c(packageGlobalsFunctions,
                unlist(lapply(strsplit(substring(externalMethods, 7L), split = ":"),
                              "[[", 1L))[unlist(lapply(externalMethods,
                              function(x) {
                                  any(unlist(eapply(get(x, packageEnv), function(y)
                                      environmentName(environment(y)) == package)))
                              }))])))

            packageGlobals <-
              lapply(packageObjs[["Name"]][packageObjs[["Type"]] == "S4MethodsTable"],
                     function(x) {
                         mEnv <- get(x, packageEnv)
                         methods <- ls(mEnv, all = TRUE)
                         inPackage <-
                           unlist(lapply(methods, function(y)
                                         environmentName(environment(get(y, mEnv))) ==
                                         package))
                         methods <- methods[inPackage]
                         z <-
                           lapply(methods, function(y)
                                  findGlobalsBioC(get(y, mEnv), merge = FALSE))
                         list("classes" = unlist(strsplit(methods, "#")),
                              "functions" = unlist(lapply(z, "[[", "functions")),
                              "variables" = unlist(lapply(z, "[[", "variables")))
                     })
            packageClasses <-
             sort(unique(c(packageClasses,
                           unlist(lapply(packageGlobals, "[[", "classes")))))
            packageGlobalsFunctions <-
              sort(unique(c(packageGlobalsFunctions,
                            unlist(lapply(packageGlobals, "[[", "functions")))))
            packageGlobalsVariables <-
              sort(unique(c(packageGlobalsVariables,
                            unlist(lapply(packageGlobals, "[[", "variables")))))
        }
        if (any(packageObjs[["Type"]] == "Other")) {
            packageGlobals <-
              lapply(packageObjs[["Name"]][packageObjs[["Type"]] == "Other" &
                                           packageObjs[["Function"]]],
                     function(x)
                     tryCatch(findGlobalsBioC(get(x, packageEnv), merge = FALSE),
                              error = function(e)
                              list("functions" = character(0L),
                                   "variables" = character(0L))))
            packageGlobalsFunctions <-
              sort(unique(c(packageGlobalsFunctions,
                            unlist(lapply(packageGlobals, "[[", "functions")))))
            packageGlobalsVariables <-
              sort(unique(c(packageGlobalsVariables,
                            unlist(lapply(packageGlobals, "[[", "variables")))))
        }
    }

    ## Remove objects that can't be found; may have been local
    packageGlobalsFunctions <-
      packageGlobalsFunctions[unlist(lapply(packageGlobalsFunctions, exists,
                                            packageEnv))]
    packageGlobalsVariables <-
      packageGlobalsVariables[unlist(lapply(packageGlobalsVariables, exists,
                                            packageEnv))]

    ## Reclassify functions that appear as variables (e.g. sapply(data, mean))
    if (length(packageGlobalsVariables) > 0L) {
        isFunction <-
          unlist(lapply(packageGlobalsVariables, function(x)
                        tryCatch(is.function(get(x, packageEnv)),
                                 error = function(e) FALSE)))
        packageGlobalsFunctions <-
          sort(unique(c(packageGlobalsFunctions,
                        packageGlobalsVariables[isFunction])))
        packageGlobalsVariables <- packageGlobalsVariables[!isFunction]
    }

    ## Find which global objects are external
    if (package %in% loadedNamespaces()) {
        packageImports <- getNamespaceImports(packageEnv)
        packageImports <-
          lapply(structure(seq_len(length(packageImports)),
                           names = names(packageImports)),
                 function(i) {
                     if (identical(packageImports[[i]], TRUE)) {
                         importedEnv <- getNamespace(names(packageImports)[i])
                         imports <- ls(importedEnv, all = TRUE)
                         importedEnvImports <- getNamespaceImports(importedEnv)
                         imports <-
                           sort(unique(c(imports, unlist(importedEnvImports[
                                            !(names(importedEnvImports) %in%
                                              names(packageImports))]))))
                         names(imports) <- imports
                         imports
                     } else {
                         packageImports[[i]]
                     }
                 })
    } else {
        packageImports <- NULL
    }
    packageOriginGlobalsFunctions <-
      unlist(lapply(packageGlobalsFunctions,
             function(x) {
                 functionList <- findFunction(x, where = packageEnv)
                 if (length(functionList) == 0L) {
                     functionList <- findFunction(x, where = globalenv())
                 }
                 candidates <-
                   sub("package:", "",
                       unlist(lapply(functionList, environmentName)))
                 if (paste("imports:", package, sep = "") %in% candidates) {
                     candidates <-
                       names(packageImports)[unlist(lapply(packageImports,
                                                    function(y) x %in% y))]
                 }
                 isS4Method <-
                   unlist(lapply(candidates, function(y)
                                 isS4(get(x, getPackageEnvironment(y)))))
                 if (any(isS4Method) && !all(isS4Method)) {
                     candidates <- candidates[isS4Method]
                 }
                 candidates[1L]
             }))

    packageClasses <-
      packageClasses[unlist(lapply(packageClasses, isClass))]
    packageExternalClasses <-
      packageClasses[!(packageClasses %in% getClasses(packageEnv))]

    externalOrigin <-
      (is.na(packageOriginGlobalsFunctions) |
       packageOriginGlobalsFunctions != package)
    packageExternalGlobalsFunctions <-
      packageGlobalsFunctions[externalOrigin]
    packageOriginGlobalsFunctions <-
      packageOriginGlobalsFunctions[externalOrigin]

    packageExternalGlobalsVariables <-
      packageGlobalsVariables[!(packageGlobalsVariables %in%
                                ls(packageEnv, all = TRUE))]

    ## Prepare the output
    S4ClassesOutput <- list()
    S4MethodsOutput <- list()
    functionsOutput <- list()
    variablesOutput <- list()

    if (length(packageExternalClasses) > 0L) {
        S4ClassesOutput <-
          split(packageExternalClasses,
                unlist(lapply(packageExternalClasses, function(x)
                       slot(getClassDef(x, where = packageEnv), "package"))))
    }

    if (length(packageExternalGlobalsFunctions) > 0L) {
        isS4Method <-
          unlist(lapply(seq_len(length(packageExternalGlobalsFunctions)),
                        function(i)
                        isS4(getFunction(packageExternalGlobalsFunctions[i],
                          where =
                          getPackageEnvironment(packageOriginGlobalsFunctions[i])))))
        if (any(isS4Method)) {
            S4MethodsOutput <-
              split(packageExternalGlobalsFunctions[isS4Method],
                    packageOriginGlobalsFunctions[isS4Method])
        }
        if (!all(isS4Method)) {
            functionsOutput <-
              split(packageExternalGlobalsFunctions[!isS4Method],
                    packageOriginGlobalsFunctions[!isS4Method])
        }
    }

    if (length(packageExternalGlobalsVariables) > 0L) {
        variablesOrigin <- lapply(packageExternalGlobalsVariables, find)
        variablesOutput <-
          split(packageExternalGlobalsVariables,
                sub("package:", "", unlist(variablesOrigin)))
    }

    list(S4Classes = S4ClassesOutput, S4Methods = S4MethodsOutput,
         functions = functionsOutput, variables = variablesOutput)
}
