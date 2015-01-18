library(codetoolsBioC)

isInstalled <- function(pkg) {
  !identical(suppressWarnings(packageDescription(pkg)), NA)
}

if (isInstalled("aroma.core")) {
  message("Package: aroma.core")

  ## aroma.core imports R.filesets::clearCache(), but that is only
  ## R.oo::clearCache() re-exported.  This will generate a warning
  ## that the clearCache() generics could not be found.
  deps <- findExternalDeps("aroma.core")
  str(deps)

} # if (isInstalled(...))
