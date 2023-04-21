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



#' Find External Dependencies of a Package
#' 
#' Finds external dependencies of a package.
#' 
#' The result is an approximation based upon the findings of \code{findGlobals}
#' and an exploration of the package namespace.
#' 
#' @param package the quoted name of the package to analyze.
#' @return A list with four components: \code{S4Classes}, \code{S4Methods},
#' \code{functions}, and \code{variables}. Each of these components in turn
#' holds a named list of character vectors of object names from external
#' packages.
#' @author Patrick Aboyoun
#' @export
#' @seealso \code{\link[codetools]{findGlobals}},
#' %\code{\link[codetools]{checkUsagePackage}}, Rd file not found.
#' @keywords programming
#' @examples
#' 
#' library(stats4)
#' findExternalDeps("stats4")
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
    pkgImpFunctions <- character(0L)
    pkgImpVariables <- character(0L)
    if (nrow(pkgObjs) > 0L) {
        ## FIXME: we may use "foreign" classes without extending them, i.e. w/o
        ##   having own S4Class objects -> search for is(), as() extends() ..?..
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
            pkgImpFunctions <-
                suniquec(pkgImpFunctions, pkgGlobals, "functions_import")
            pkgImpVariables <-
                suniquec(pkgImpVariables, pkgGlobals, "variables_import")
        }
    }

    ## Remove objects that can't be found; may have been local
    idx <- vapply(pkgGlobFunctions, FUN=exists, FUN.VALUE=NA, envir=pkgEnv)
    pkgGlobFunctions <- pkgGlobFunctions[idx]
    idx <- vapply(pkgGlobVariables, FUN=exists, FUN.VALUE=NA, envir=pkgEnv)
    pkgGlobVariables <- pkgGlobVariables[idx]

    ## Reclassify functions that appear as variables (e.g. sapply(data, mean))
    if (length(pkgGlobVariables) > 0L) {
        isFunction <- vapply(pkgGlobVariables, FUN=function(x)
            tryCatch(is.function(get(x, pkgEnv)), error=function(e) FALSE),
                             FUN.VALUE=NA)
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
              warning("method(s) for '", x, "' imported from package(s) ",
                   paste(candidates, collapse=", "), ", but none of them ",
                   "define the generic. This may be because the method(s) ",
                   "is/are re-exported by those packages.")
              "<unknown>"  ## NA_character_ is used for other purposes
            } else {
              candidates <- candidates[candidatesDefined]
              isS4Method <- vapply(candidates, function(y)
                  isS4(get(x, getPackageEnvironment(y))), NA)
              if (any(isS4Method) && !all(isS4Method)) {
                  candidates <- candidates[isS4Method]
              }
              candidates[1L]
            }
        })

    pkgClasses <-
        pkgClasses[vapply(pkgClasses, isClass, NA)]
    pkgExternalClasses <-
        pkgClasses[!(pkgClasses %in% getClasses(pkgEnv))]

    externalOrigin <- (is.na(pkgOriGlobFunctions) |
        (pkgOriGlobFunctions != package)) &
        (pkgOriGlobFunctions != "<unknown>")
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

    ## Functions and objects imported via :: and :::
    if (length(pkgImpFunctions) > 0L) {
      objs <- strsplit(pkgImpFunctions, split="(::|:::)")
      for (parts in objs) {
        pkg <- parts[1L]
        obj <- parts[2L]
        functionsOutput[[pkg]] <- c(functionsOutput[[pkg]], obj)
      }
      functionsOutput <- lapply(functionsOutput, FUN=function(x) sort(unique(x)))
    }

    if (length(pkgImpVariables) > 0L) {
      objs <- strsplit(pkgImpVariables, split="(::|:::)")
      for (parts in objs) {
        pkg <- parts[1L]
        obj <- parts[2L]
        variablesOutput[[pkg]] <- c(variablesOutput[[pkg]], obj)
      }
      variablesOutput <- lapply(variablesOutput, FUN=function(x) sort(unique(x)))
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
