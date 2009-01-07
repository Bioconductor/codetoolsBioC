\name{writeNamespaceImports}
\alias{writeNamespaceImports}
\title{Write NAMESPACE file imports statements}
\usage{
writeNamespaceImports(package, file = "")
}
\arguments{
  \item{package}{character naming package to analyze.}
  \item{file}{either a character string naming a file or an open writing
    connection. \code{""} indicates writing to the console.}
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
