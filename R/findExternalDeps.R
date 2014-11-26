loadRequiredPackages <- function(package)
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
    pkgEnv <- getPackageEnvironment(package)
    pkgObjs <- getPackageTable(package)
    pkgObjs <-
        pkgObjs[pkgObjs[["Origin"]] == package |
                pkgObjs[["Type"]] == "S4MethodsTable", , drop = FALSE]

    ## Cast wide net to find classes, functions, and variables
    pkgClasses <- character(0L)
    pkgGlobFunctions <- character(0L)
    pkgGlobVariables <- character(0L)
    if (nrow(pkgObjs) > 0L) {
        if (any(pkgObjs[["Type"]] == "S4Class")) {
            idx <- pkgObjs[["Type"]] == "S4Class"
            S4Classes <- pkgObjs[["Name"]][idx]
            nearbyClasses <- ulapply(S4Classes, function(x) {
                classDef <- get(x, pkgEnv)
                containedClasses <- slot(classDef, "contains")
                distance <- ulapply(containedClasses, slot, "distance")
                c(vapply(slot(classDef, "slots"), unname, ""),
                  names(containedClasses[distance == 1L]))
            })
            pkgClasses <- suniquec(pkgClasses, nearbyClasses)
        }
        S3MethodsTable <- get(".__S3MethodsTable__.", pkgEnv)
        if (length(S3Methods <- ls(S3MethodsTable, all.names=TRUE)) > 0L) {
            pkgGlobals <- lapply(S3Methods, function(x)
                findGlobalsBioC(get(x, S3MethodsTable), merge = FALSE))
            pkgGlobFunctions <-
                suniquec(pkgGlobFunctions, pkgGlobals, "functions")
            pkgGlobVariables <-
                suniquec(pkgGlobVariables, pkgGlobals, "variables")
        }
        if (any(idx <- pkgObjs[["Type"]] == "S4MethodsTable")) {
            idx <- idx & pkgObjs[["Origin"]] != package
            externalMethods <- pkgObjs[["Name"]][idx]
            idx <- vapply(externalMethods, function(x) {
                any(unlist(eapply(get(x, pkgEnv), function(y)
                    environmentName(environment(y)) == package)))
            }, NA)
            X0 <- strsplit(substring(externalMethods, 7L), split = ":")
            X <- ulapply(X0, "[[", 1L)[idx]
            pkgGlobFunctions <- suniquec(pkgGlobFunctions, X)

            idx <- pkgObjs[["Type"]] == "S4MethodsTable"
            S4Methods <- pkgObjs[["Name"]][idx]
            pkgGlobals <- lapply(S4Methods, function(x) {
                mEnv <- get(x, pkgEnv)
                methods <- ls(mEnv, all.names = TRUE)
                inPackage <- vapply(methods, function(y)
                    environmentName(environment(get(y, mEnv))) == package, NA)
                methods <- methods[inPackage]
                z <- lapply(methods, function(y)
                    findGlobalsBioC(get(y, mEnv), merge = FALSE))
                list("classes" = unlist(strsplit(methods, "#")),
                     "functions" = ulapply(z, "[[", "functions"),
                     "variables" = ulapply(z, "[[", "variables"))
            })
            pkgClasses <-
                suniquec(pkgClasses, pkgGlobals, "classes")
            pkgGlobFunctions <-
                suniquec(pkgGlobFunctions, pkgGlobals, "functions")
            pkgGlobVariables <-
                suniquec(pkgGlobVariables, pkgGlobals, "variables")
        }
        if (any(pkgObjs[["Type"]] == "Other")) {
            idx <- pkgObjs[["Type"]] == "Other" &
            pkgObjs[["Function"]]
            Other <- pkgObjs[["Name"]][idx]
            pkgGlobals <- lapply(Other, function(x) tryCatch({
                findGlobalsBioC(get(x, pkgEnv), merge = FALSE)
            }, error = function(e) {
                list("functions" = character(0L),
                     "variables" = character(0L))
            }))
            pkgGlobFunctions <-
                suniquec(pkgGlobFunctions, pkgGlobals, "functions")
            pkgGlobVariables <-
                suniquec(pkgGlobVariables, pkgGlobals, "variables")
        }
    }

    ## Remove objects that can't be found; may have been local
    idx <- vapply(pkgGlobFunctions, exists, NA, pkgEnv)
    pkgGlobFunctions <- pkgGlobFunctions[idx]
    idx <- vapply(pkgGlobVariables, exists, NA, pkgEnv)
    pkgGlobVariables <- pkgGlobVariables[idx]

    ## Reclassify functions that appear as variables (e.g. sapply(data, mean))
    if (length(pkgGlobVariables) > 0L) {
        isFunction <- vapply(pkgGlobVariables, function(x)
            tryCatch(is.function(get(x, pkgEnv)), error=function(e) FALSE),
                             NA)
        pkgGlobFunctions <-
            suniquec(pkgGlobFunctions,
                     pkgGlobVariables[isFunction])
        pkgGlobVariables <- pkgGlobVariables[!isFunction]
    }

    ## Find which global objects are external
    pkgImports <-
        if (package %in% loadedNamespaces()) {
            pkgImp <- getNamespaceImports(pkgEnv)
            X <- setNames(seq_along(pkgImp), names(pkgImp))
            lapply(X, function(i) {
                if (identical(pkgImp[[i]], TRUE)) {
                    importedEnv <- getNamespace(names(pkgImp)[i])
                    imports <- ls(importedEnv, all.names = TRUE)
                    importedEnvImports <- getNamespaceImports(importedEnv)
                    idx <- !(names(importedEnvImports) %in% names(pkgImp))
                    imports <- suniquec(imports, unlist(importedEnvImports[idx]))
                    setNames(imports, imports)
                } else {
                    pkgImp[[i]]
                }
            })
        } ## else NULL
    pkgOriGlobFunctions <-
        ulapply(pkgGlobFunctions, function(x) {
            functionList <- findFunction(x, where = pkgEnv)
            if (length(functionList) == 0L) {
                functionList <- findFunction(x, where = globalenv())
            }
            candidates <-
                sub("package:", "", ulapply(functionList, environmentName))
            if (paste0("imports:", package) %in% candidates) {
                idx <- ulapply(pkgImports, function(y) x %in% y)
                candidates <- names(pkgImports)[idx]
            }
            candidatesDefined <- ulapply(candidates, function(y) {
              exists(x, getPackageEnvironment(y), inherits=FALSE) |
                  isS4(get(x, getPackageEnvironment(y)))
            })
            if (!any(candidatesDefined)) {
              stop("method(s) for '", x, "' imported from package(s) ",
                   paste(candidates, collapse=", "), ", but none of them ",
                   "define the generic")
            }
            candidates <- candidates[candidatesDefined]
            isS4Method <- vapply(candidates, function(y)
                isS4(get(x, getPackageEnvironment(y))), NA)
            if (any(isS4Method) && !all(isS4Method)) {
                candidates <- candidates[isS4Method]
            }
            candidates[1L]
        })

    pkgClasses <-
        pkgClasses[vapply(pkgClasses, isClass, NA)]
    pkgExternalClasses <-
        pkgClasses[!(pkgClasses %in% getClasses(pkgEnv))]

    externalOrigin <- is.na(pkgOriGlobFunctions) |
        (pkgOriGlobFunctions != package)
    pkgExtGlobFunctions <-
        pkgGlobFunctions[externalOrigin]
    pkgOriGlobFunctions <-
        pkgOriGlobFunctions[externalOrigin]

    externalGlobalsVariables <-
        !(pkgGlobVariables %in% ls(pkgEnv, all.names = TRUE))
    pkgExtGlobVariables <-
        pkgGlobVariables[externalGlobalsVariables]

    ## Prepare the output
    S4ClassesOutput <- list()
    S4MethodsOutput <- list()
    functionsOutput <- list()
    variablesOutput <- list()

    if (length(pkgExternalClasses) > 0L) {
        package <- ulapply(pkgExternalClasses, function(x) {
            slot(getClassDef(x, where = pkgEnv), "package")
        })
        S4ClassesOutput <- split(pkgExternalClasses, package)
    }

    if (length(pkgExtGlobFunctions) > 0L) {
        isS4Method <-
            vapply(seq_along(pkgExtGlobFunctions), function(i) {
                env <- getPackageEnvironment(pkgOriGlobFunctions[i])
                isS4(getFunction(pkgExtGlobFunctions[i], where = env))
            }, NA)
        if (any(isS4Method)) {
            S4MethodsOutput <-
                split(pkgExtGlobFunctions[isS4Method],
                      pkgOriGlobFunctions[isS4Method])
        }
        if (!all(isS4Method)) {
            functionsOutput <-
                split(pkgExtGlobFunctions[!isS4Method],
                      pkgOriGlobFunctions[!isS4Method])
        }
    }

    if (length(pkgExtGlobVariables) > 0L) {
        variablesOrigin <- lapply(pkgExtGlobVariables, find)
        package <- sub("package:", "", unlist(variablesOrigin))
        variablesOutput <-
            split(pkgExtGlobVariables, package)
    }

    list(S4Classes = S4ClassesOutput, S4Methods = S4MethodsOutput,
         functions = functionsOutput, variables = variablesOutput)
}
