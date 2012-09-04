getRdFileNames <- function(topic, package=NULL)
{
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

   paths <- sapply(.find.package(package, lib.loc), function(p) {
       utils:::index.search(topic, p)
   })
   paths <- unlist(paths)
   
   data.frame(packages=basename(names(paths)), Rd.file=basename(paths))
 }
 
