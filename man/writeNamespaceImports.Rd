\name{writeNamespaceImports}
\alias{writeNamespaceImports}
\title{Write NAMESPACE file imports statements}
\usage{
writeNamespaceImports(package, file = "", append = FALSE, quote = FALSE,
                      ignoreAllBasePackages = FALSE)
}
\arguments{
  \item{package}{character naming package to analyze.}
  \item{file}{either a character string naming a file or an open writing
    connection. \code{""} indicates writing to the console.}
  \item{append}{logical. If \code{TRUE}, the output is appended to the file.
    If \code{FALSE}, any existing file with the specified name is destroyed
    and a new one is created. This argument is only used if \code{file}
    is a character string.}
  \item{quote}{logical. If \code{TRUE}, all import fields will be double-quoted.
    If \code{FALSE}, only non-standard names like \code{[<-.fractions} will be
    double-quoted.}
  \item{ignoreAllBasePackages}{logical. If \code{TRUE}, all base packages
    ('stats', 'graphics', 'grDevices', 'utils', 'datasets', 'methods', 'base')
    are ignored in the output. If \code{FALSE}, only package 'base' is ignored.
    See Details section below.}
}
\description{
  Writes imports statements that can be included in a package's NAMESPACE file.
}
\details{
  The result is an approximation based upon the findings of
  \code{\link{findExternalDeps}}. S4 class dependencies are embedded into
  importClassesFrom statements, S4 methods dependencies are embedded into
  importMethodsFrom statements, and all other dependencies are embedded into
  importFrom statements.

  Since a package with a namespace implicitly imports the namespace from
  package 'base', the 'base' package is always ignored by
  \code{writeNamespaceImports}. While the other base packages ('stats',
  'graphics', 'grDevices', 'utils', 'datasets', 'methods') are available in a
  standard R session, there is no guarantee they will be on the search list.
  As a result, the default setting of the \code{ignoreAllBasePackages} argument
  is \code{FALSE} so these other packages are represented in import*From
  statements. It is, however, possible to set \code{ignoreAllBasePackages}
  argument to \code{TRUE} to suppress any base package from being included in
  the import*From statements.

  See the document "Writing R Extensions" that is hosted on CRAN for more
  details on the proper construction of a NAMESPACE file.  
}
\author{Patrick Aboyoun}
\seealso{
  \code{\link{findExternalDeps}}
}
\examples{
library(stats4)
writeNamespaceImports("stats4")
}
\keyword{programming}
