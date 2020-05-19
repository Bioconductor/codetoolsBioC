#' Write an R code file containing a .test function to run RUnit tests
#' 
#' This function generates an R code file containing a single function
#' \code{.test} that can be used to run the RUnit unit tests of a package. The
#' generated \code{.test} function is customized for the specified package and
#' takes care of setting up RUnit options, configuring the location of unit
#' test files, and generating helpful summary data when tests fail.
#' 
#' 
#' Copy the generated file, or its contents, into your package source.  You can
#' add an additional R script in pkg/tests that loads your package and then
#' calls \code{pkg:::.test()}.  This will ensure that unit tests are run during
#' R CMD check and that a unit test failure will result in an ERROR report from
#' R CMD check.
#' 
#' @param pkg Package name: a \code{.test} function will be generated
#' customized for the specified package name.
#' @param file Determines the file where code for the \code{.test} function
#' will be written.
#' @return Called for the side-effect of creating a file containing the
#' \code{.test} function definition customized for \code{pkg}.
#' @author Seth Falcon
#' @export
#' @examples
#' 
#'   tf <- tempfile()
#'   writeRUnitRunner("foo", tf)
#'   lines <- readLines(tf)
#'   file.remove(tf)
#' 
writeRUnitRunner <- function(pkg, file = "runit_runner.R")
{
    line <- paste0("BiocGenerics:::testPackage(\"", pkg, "\")")
    writeLines(line, file)
}
