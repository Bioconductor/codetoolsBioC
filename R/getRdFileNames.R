#' Track Rd file names at which 'topic' is documented
#' 
#' Tracks the Rd file names at which a given 'topic' (alias) is documented.
#' 
#' @param topic A length-one character vector specifying the topic (alias).
#' @param package A character vector given the packages to search for Rd file
#' names that document the \code{topic} , or 'NULL'. By default, all the
#' packages in the search path are used.
#' @author Chao-Jen Wong \email{cwon2@@fhcrc.org}
#' @export
#' @keywords programming
#' @examples
#' 
#' getRdFileNames("rbind")
#' 
#' isInstalled <- function(pkg)
#'   inherits(suppressWarnings(packageDescription(pkg)), "packageDescription")
#' 
#' if (isInstalled("IRanges"))
#'   getRdFileNames("rbind", package=c("base", "IRanges"))
#' 
#' if (isInstalled("Biobase"))
#'   getRdFileNames("ExpressionSet", "Biobase")
#' 
getRdFileNames <- function(topic, package=NULL)
{
   ## Imports
   index.search <- utils:::index.search

   ## example 1: getRd("ExpressionSet", "Biobase")
   ## example 2: getRd("pData")

   if (missing(topic))
       stop("'topic' must be given")
   if (length(topic) > 1)
       stop("'topic' should be a length-one character vector. ")

   topic <- as.character(topic)
   lib.loc <- .libPaths()

   if (!is.null(package) & !is.character(package))
       stop("'package' should be a character vector.")

   if (is.null(package))
       package <- .packages(all.available = TRUE, lib.loc = lib.loc)

   paths <- sapply(find.package(package, lib.loc), function(p) {
       index.search(topic, p)
   })
   paths <- unlist(paths)

   data.frame(packages=basename(names(paths)), Rd.file=basename(paths))
}
