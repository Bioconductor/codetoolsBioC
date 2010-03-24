writeRUnitRunner <- function(pkg, file = "runit_runner.R")
{
    f <- system.file("templates/runit_runner.R.in", package = "codetoolsBioC")
    lines <- readLines(f)
    lines <- gsub("@PKG@", pkg, lines, fixed = TRUE)
    writeLines(lines, file)
}

