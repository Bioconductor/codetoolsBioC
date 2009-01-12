writeNamespaceImports <- function(package, file = "", append = FALSE) {
    writeImports <- function(x, prefix, file) {
        spaces <- paste(rep(" ", nchar(prefix) + 1), collapse = "")
        for (i in seq_len(length(x))[!(names(x) %in%
                                       c("base", "Autoloads"))]) {
            qstring <- rep("", length(x[[i]]))
            qstring[grep("<-$", x[[i]])] <- "\""
            cat(paste(prefix, "(", names(x[i]), ",\n", spaces,
                      paste(qstring, x[[i]], qstring, sep = "",
                      collapse = paste(",\n", spaces, sep = "")),
                            ")", sep = ""), file = file)
            cat("\n\n", file = file)
        }
    }
    if (file == "") 
        file <- stdout()
    else if (is.character(file)) {
        file <- file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
    }
    else if (!isOpen(file, "w")) {
        open(file, "w")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("'file' must be a character string or connection")
    deps <- findExternalDeps(package)
    writeImports(deps[["S4Classes"]], "importClassesFrom", file)
    writeImports(deps[["S4Methods"]], "importMethodsFrom", file)
    writeImports(deps[["functions"]], "importFrom", file)
    writeImports(deps[["variables"]], "importFrom", file)
}
