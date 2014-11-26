getPackageTable <- function(package) {
    pname <- paste("package", package, sep = ":")
    if (! pname %in% search())
        stop("package must be loaded")
    if (package %in% loadedNamespaces())
        packageEnv <- getNamespace(package)
    else
        packageEnv <- as.environment(pname)

    objFullName <- ls(packageEnv, all.names = TRUE)
    splitName <- strsplit(objFullName, split = ":")
    objName <- vapply(splitName, "[", "", 1L)

    objType <-
        unname(c(".__C__" = "S4Class", ".__M__" = "S4Methods",
                 ".__T__" = "S4MethodsTable")[substring(objName, 1L, 6L)])
    objType[is.na(objType)] <- "Other"
    objType[objName == ".__NAMESPACE__."] <- "NAMESPACE"
    objType[objName == ".__S3MethodsTable__."] <- "S3MethodsTable"

    objOrigin <- ulapply(splitName, "[", 2L)
    if (any(objType == "S4Class")) {
        isS4Class <- objType == "S4Class"
        objOrigin[isS4Class] <-
            ulapply(substring(objName[isS4Class], 7L), function(x) {
                environmentName(findClass(x, packageEnv)[[1L]])
            })
    }
    objOrigin[is.na(objOrigin)] <- package

    objIsFunction <-
        vapply(objFullName, function(x) is.function(get(x, packageEnv)), NA)
    objIsS4 <-
        vapply(objFullName, function(x) isS4(get(x, packageEnv)), NA)

    data.frame("Name" = objFullName, "Origin" = objOrigin,
               "Type" = factor(objType), "Function" = objIsFunction,
               "IsS4" = objIsS4, stringsAsFactors = FALSE)
}
