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
    are ignored in the output. If \code{FALSE}, only package 'base' is ignored.}
}
\description{
  Writes imports statements that can be included in a package's NAMESPACE file.
}
\details{
  The result is an approximation based upon the findings of
  \code{\link{findExternalDeps}}.
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
