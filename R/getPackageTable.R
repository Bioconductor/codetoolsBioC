getPackageTable <- function(package) {
    pname <- paste("package", package, sep = ":")
    if (! pname %in% search())
        stop("package must be loaded")
    if (package %in% loadedNamespaces())
        packageEnv <- getNamespace(package)
    else
        packageEnv <- as.environment(pname)

    objFullName <- ls(packageEnv, all = TRUE)
    splitName <- strsplit(objFullName, split = ":")
    objName <- unlist(lapply(splitName, "[", 1L))

    objType <-
      unname(c(".__C__" = "S4Class", ".__M__" = "S4Methods",
               ".__T__" = "S4MethodsTable")[substring(objName, 1L, 6L)])
    objType <- ifelse(is.na(objType), "Other", objType)
    objType[objName == ".__NAMESPACE__."] <- "NAMESPACE"
    objType[objName == ".__S3MethodsTable__."] <- "S3MethodsTable"

    objOrigin <- unlist(lapply(splitName, "[", 2L))
    if (any(objType == "S4Class")) {
        objOrigin[objType == "S4Class"] <-
          unlist(lapply(substring(objName[objType == "S4Class"], 7L),
                        function(x)
                        environmentName(findClass(x, packageEnv)[[1L]])))
    }
    objOrigin[is.na(objOrigin)] <- package

    objIsFunction <-
      unlist(lapply(objFullName, function(x) is.function(get(x, packageEnv))))
    objIsS4 <-
      unlist(lapply(objFullName, function(x) isS4(get(x, packageEnv))))

    data.frame("Name" = objFullName, "Origin" = objOrigin,
               "Type" = factor(objType), "Function" = objIsFunction,
               "IsS4" = objIsS4, stringsAsFactors = FALSE)
}
