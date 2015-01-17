library(codetoolsBioC)

message("Package: stats")
library(stats)
deps <- findExternalDeps("stats")
str(deps)
# Should pick up MASS::gamma.shape as well
str(deps$functions$MASS)
