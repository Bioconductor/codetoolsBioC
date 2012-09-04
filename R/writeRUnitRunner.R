writeRUnitRunner <- function(pkg, file = "runit_runner.R")
{
    line <- paste0("BiocGenerics:::testPackage(\"", pkg, "\")")
    writeLines(line, file)
}
