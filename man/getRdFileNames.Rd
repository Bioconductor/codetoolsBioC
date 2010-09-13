\name{getRdFileNames}

\alias{getRdFileNames}
\title{Track Rd file names at which 'topic' is documented}

\description{
Tracks the Rd file names at which a given 'topic' (alias) is documented.}
\usage{
getRdFileNames(topic, package=NULL)
}

\arguments{
  \item{topic}{A length-one character vector specifying the topic (alias).}
  \item{package}{A character vector given the packages to search for
    Rd file names that document the \code{topic} , or 'NULL'. By
    default, all the packages in the search path are used.}        
}

\author{Chao-Jen Wong \email{cwon2@fhcrc.org}}

\examples{
getRdFileNames("rbind")

getRdFileNames("rbind", package=c("base", "IRanges"))

getRdFileNames("ExpressionSet", "Biobase")
}

\keyword{programming}
