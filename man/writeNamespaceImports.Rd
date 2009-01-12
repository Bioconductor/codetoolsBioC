\name{writeNamespaceImports}
\alias{writeNamespaceImports}
\title{Write NAMESPACE file imports statements}
\usage{
writeNamespaceImports(package, file = "", append = FALSE)
}
\arguments{
  \item{package}{character naming package to analyze.}
  \item{file}{either a character string naming a file or an open writing
    connection. \code{""} indicates writing to the console.}
  \item{append}{If \code{TRUE}, the output is appended to the file. If
    \code{FALSE}, any existing file with the specified name is destroyed
    and a new one is created. This argument is only used if \code{file}
    is a character string.}
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
