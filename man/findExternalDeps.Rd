\name{findExternalDeps}
\alias{findExternalDeps}
\title{Find External Dependencies of a Package}
\usage{
findExternalDeps(package) 
}
\arguments{
  \item{package}{the quoted name of the package to analyze.}
}
\value{
  A list with four components:  \code{S4Classes}, \code{S4Methods},
  \code{functions}, and \code{variables}. Each of these components in
  turn holds a named list of character vectors of object names from
  external packages. 
}
\description{
  Finds external dependencies of a package.
}
\details{
  The result is an approximation based upon the findings of
  \code{findGlobals} and an exploration the package namespace.
}
\author{Patrick Aboyoun}
\seealso{
  \code{\link[codetools]{findGlobals}}, \code{\link[codetools]{checkUsagePackage}}
}
\examples{
library(stats4)
findExternalDeps("stats4")
}
\keyword{programming}
