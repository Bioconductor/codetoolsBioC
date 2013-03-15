loadRequiredPackages <-
    function(package)
    ## all Depends: and Imports packages need to be on the search path
{
    requiredPackages <- local({
        desc <- packageDescription(package)
        x <- desc[names(desc) %in% c("Imports", "Depends")]
        regex <- "([[:space:]]*|\\([^\\)]+\\))"
        x <- unique(c(package, unlist(strsplit(gsub(regex, "", x), ","))))
        x[x != "R"]
    })
    idx <- !requiredPackages %in% sub("package:", "",search())
    if (any(idx)) {
        message("attaching required packages '",
                paste0(requiredPackages[idx], collapse="', '"),
                "'")
        for (pkg in requiredPackages[idx])
            require(pkg, character.only=TRUE)
    }        
}

findExternalDeps <- function(package) {
    loadRequiredPackages(package)
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
            idx <- packageObjs[["Type"]] == "S4Class"
            S4Classes <- packageObjs[["Name"]][idx]
            nearbyClasses <- ulapply(S4Classes, function(x) {
                classDef <- get(x, packageEnv)
                containedClasses <- slot(classDef, "contains")
                distance <- ulapply(containedClasses, slot, "distance")
                c(ulapply(slot(classDef, "slots"), unname),
                  names(containedClasses[distance == 1L]))
            })
            packageClasses <- suniquec(packageClasses, nearbyClasses)
        }
        S3MethodsTable <- get(".__S3MethodsTable__.", packageEnv)
        S3Methods <- ls(S3MethodsTable, all=TRUE)
        if (length(S3Methods) > 0L) {
            packageGlobals <- lapply(S3Methods, function(x) {
                findGlobalsBioC(get(x, S3MethodsTable), merge = FALSE)
            })
            packageGlobalsFunctions <-
                suniquec(packageGlobalsFunctions, packageGlobals, "functions")
            packageGlobalsVariables <-
                suniquec(packageGlobalsVariables, packageGlobals, "variables")
        }
        if (any(packageObjs[["Type"]] == "S4MethodsTable")) {
            idx <- (packageObjs[["Origin"]] != package &
                    packageObjs[["Type"]] == "S4MethodsTable")
            externalMethods <- packageObjs[["Name"]][idx]
            idx <- ulapply(externalMethods, function(x) {
                any(unlist(eapply(get(x, packageEnv), function(y) {
                    environmentName(environment(y)) == package
                })))
            })
            X0 <- strsplit(substring(externalMethods, 7L), split = ":")
            X <- ulapply(X0, "[[", 1L)[idx]
            packageGlobalsFunctions <- suniquec(packageGlobalsFunctions, X)

            idx <- packageObjs[["Type"]] == "S4MethodsTable"
            S4Methods <- packageObjs[["Name"]][idx]
            packageGlobals <- lapply(S4Methods, function(x) {
                mEnv <- get(x, packageEnv)
                methods <- ls(mEnv, all = TRUE)
                inPackage <- ulapply(methods, function(y) {
                    environmentName(environment(get(y, mEnv))) == package
                })
                methods <- methods[inPackage]
                z <- lapply(methods, function(y) {
                    findGlobalsBioC(get(y, mEnv), merge = FALSE)
                })
                list("classes" = unlist(strsplit(methods, "#")),
                     "functions" = ulapply(z, "[[", "functions"),
                     "variables" = ulapply(z, "[[", "variables"))
            })
            packageClasses <-
                suniquec(packageClasses, packageGlobals, "classes")
            packageGlobalsFunctions <-
                suniquec(packageGlobalsFunctions, packageGlobals, "functions")
            packageGlobalsVariables <-
                suniquec(packageGlobalsVariables, packageGlobals, "variables")
        }
        if (any(packageObjs[["Type"]] == "Other")) {
            idx <- packageObjs[["Type"]] == "Other" &
            packageObjs[["Function"]]
            Other <- packageObjs[["Name"]][idx]
            packageGlobals <- lapply(Other, function(x) tryCatch({
                findGlobalsBioC(get(x, packageEnv), merge = FALSE)
            }, error = function(e) {
                list("functions" = character(0L),
                     "variables" = character(0L))
            }))
            packageGlobalsFunctions <-
                suniquec(packageGlobalsFunctions, packageGlobals, "functions")
            packageGlobalsVariables <-
                suniquec(packageGlobalsVariables, packageGlobals, "variables")
        }
    }

    ## Remove objects that can't be found; may have been local
    idx <- ulapply(packageGlobalsFunctions, exists, packageEnv)
    packageGlobalsFunctions <- packageGlobalsFunctions[idx]
    idx <- ulapply(packageGlobalsVariables, exists, packageEnv)
    packageGlobalsVariables <- packageGlobalsVariables[idx]

    ## Reclassify functions that appear as variables (e.g. sapply(data, mean))
    if (length(packageGlobalsVariables) > 0L) {
        isFunction <- ulapply(packageGlobalsVariables, function(x) tryCatch({
            is.function(get(x, packageEnv))
        }, error = function(e) {
            FALSE
        }))
        packageGlobalsFunctions <-
            suniquec(packageGlobalsFunctions,
                     packageGlobalsVariables[isFunction])
        packageGlobalsVariables <- packageGlobalsVariables[!isFunction]
    }

    ## Find which global objects are external
    if (package %in% loadedNamespaces()) {
        packageImports <- getNamespaceImports(packageEnv)
        X <- setNames(seq_len(length(packageImports)),
                      names(packageImports))
        packageImports <- lapply(X, function(i) {
            if (identical(packageImports[[i]], TRUE)) {
                importedEnv <- getNamespace(names(packageImports)[i])
                imports <- ls(importedEnv, all = TRUE)
                importedEnvImports <- getNamespaceImports(importedEnv)
                idx <- !(names(importedEnvImports) %in% names(packageImports))
                imports <- suniquec(imports, unlist(importedEnvImports[idx]))
                setNames(imports, imports)
            } else {
                packageImports[[i]]
            }
        })
    } else {
        packageImports <- NULL
    }
    packageOriginGlobalsFunctions <-
        ulapply(packageGlobalsFunctions, function(x) {
            functionList <- findFunction(x, where = packageEnv)
            if (length(functionList) == 0L) {
                functionList <- findFunction(x, where = globalenv())
            }
            candidates <-
                sub("package:", "", ulapply(functionList, environmentName))
            if (paste0("imports:", package) %in% candidates) {
                idx <- ulapply(packageImports, function(y) x %in% y)
                candidates <- names(packageImports)[idx]
            }
            candidates <- candidates[ulapply(candidates, function(y) {
                exists(x, getPackageEnvironment(y), inherits=FALSE)
            })]
            isS4Method <- ulapply(candidates, function(y) {
                isS4(get(x, getPackageEnvironment(y)))
            })
            if (any(isS4Method) && !all(isS4Method)) {
                candidates <- candidates[isS4Method]
            }
            candidates[1L]
        })

    packageClasses <-
        packageClasses[ulapply(packageClasses, isClass)]
    packageExternalClasses <-
        packageClasses[!(packageClasses %in% getClasses(packageEnv))]

    externalOrigin <- (is.na(packageOriginGlobalsFunctions) |
                       packageOriginGlobalsFunctions != package)
    packageExternalGlobalsFunctions <-
        packageGlobalsFunctions[externalOrigin]
    packageOriginGlobalsFunctions <-
        packageOriginGlobalsFunctions[externalOrigin]

    externalGlobalsVariables <-
        !(packageGlobalsVariables %in% ls(packageEnv, all = TRUE))
    packageExternalGlobalsVariables <-
        packageGlobalsVariables[externalGlobalsVariables]

    ## Prepare the output
    S4ClassesOutput <- list()
    S4MethodsOutput <- list()
    functionsOutput <- list()
    variablesOutput <- list()

    if (length(packageExternalClasses) > 0L) {
        package <- ulapply(packageExternalClasses, function(x) {
            slot(getClassDef(x, where = packageEnv), "package")
        })
        S4ClassesOutput <- split(packageExternalClasses, package)
    }

    if (length(packageExternalGlobalsFunctions) > 0L) {
        isS4Method <-
            ulapply(seq_along(packageExternalGlobalsFunctions), function(i) {
                env <- getPackageEnvironment(packageOriginGlobalsFunctions[i])
                isS4(getFunction(packageExternalGlobalsFunctions[i],
                                 where = env))
            })
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
        package <- sub("package:", "", unlist(variablesOrigin))
        variablesOutput <-
            split(packageExternalGlobalsVariables, package)
    }

    list(S4Classes = S4ClassesOutput, S4Methods = S4MethodsOutput,
         functions = functionsOutput, variables = variablesOutput)
}
