library(codetoolsBioC)

message("Package: tools")
deps <- findExternalDeps("tools")
str(deps)
# Should pick up utils::installed.packages
str(deps$functions$utils)
cat(sprintf("Uses 'utils::installed.packages': %s\n", is.element("installed.packages", deps$functions$utils)))
