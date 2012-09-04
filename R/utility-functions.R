getPackageEnvironment <- function(package) {
    pname <- paste("package", package, sep = ":")
    if (! pname %in% search())
        stop(package, " must be loaded")
    if (package %in% loadedNamespaces())
        getNamespace(package)
    else
        as.environment(pname)
}

## common idioms
ulapply <- function(...) unlist(lapply(...))

suniquec <-
    function(X, Y, what)
    ## optionally subset Y by elements named 'what', then make unique
    ## set with X
{
    if (!missing(what))
        Y <- ulapply(Y, "[[", what)
    sort(unique(c(X, Y)))
}
