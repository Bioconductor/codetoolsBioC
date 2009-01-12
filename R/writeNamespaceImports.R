writeNamespaceImports <-
function(package, file = "", append = FALSE, quote = FALSE, ignoreAllBasePackages = FALSE) {
    writeImports <- function(x, prefix, file) {
        spaces <- paste(rep(" ", nchar(prefix) + 1), collapse = "")
        for (i in seq_len(length(x))[!(names(x) %in% ignoredPackages)]) {
            if (quote) {
                qstring1 <- "\""
                qstring2 <- "\""
            } else {
                qstring1 <- ""
                qstring2 <- rep("", length(x[[i]]))
                qstring2[grep("<-", x[[i]])] <- "\""
            }
            cat(paste(prefix, "(", qstring1, names(x[i]), qstring1, ",\n",
                      spaces, paste(qstring2, x[[i]], qstring2, sep = "",
                                    collapse = paste(",\n", spaces, sep = "")),
                      ")", sep = ""), file = file)
            cat("\n\n", file = file)
        }
    }
    if (ignoreAllBasePackages) {
        ignoredPackages <-
          c("stats", "graphics", "grDevices", "utils", "datasets", "methods",
            "base", "Autoloads")
    } else {
        ignoredPackages <- c("base", "Autoloads")
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
