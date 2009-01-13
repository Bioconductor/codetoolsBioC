\name{writeNamespaceImports}
\alias{writeNamespaceImports}
\title{Write NAMESPACE file imports statements}
\usage{
writeNamespaceImports(package, file = "", append = TRUE, quote = FALSE)
}
\arguments{
  \item{package}{the quoted name of the package to analyze.}
  \item{file}{either a character string naming a file or an open writing
    connection. \code{""} indicates writing to the console.}
  \item{append}{logical. If \code{TRUE}, the output is appended to the file.
    If \code{FALSE}, any existing file with the specified name is destroyed
    and a new one is created. This argument is only used if \code{file}
    is a character string.}
  \item{quote}{logical. If \code{TRUE}, all import fields will be double-quoted.
    If \code{FALSE}, only non-standard names like \code{[<-.fractions} will be
    double-quoted.}
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
