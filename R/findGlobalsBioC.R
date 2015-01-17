makeUsageCollectorBioC <- function(fun, ..., name = NULL,
        enterLocal = codetools:::doNothing,
        enterGlobal = codetools:::doNothing,
        enterInternal = codetools:::doNothing,
        startCollectLocals = codetools:::doNothing,
        finishCollectLocals = codetools:::doNothing,
        warn = codetools:::warning0,
        signal = codetools:::signalUsageIssue,
        leaf = codetools:::collectUsageLeaf) {

    ## Imports
    collectUsageCall <- codetools:::collectUsageCall
    getCollectUsageHandler <- codetools:::getCollectUsageHandler
    collectUsageIsLocal <- codetools:::collectUsageIsLocal

    if (typeof(fun) != "closure")
        stop("only works for closures")

    makeCodeWalker(..., name = name,
            enterLocal = enterLocal,
            enterGlobal = enterGlobal,
            enterInternal = enterInternal,
            startCollectLocals = startCollectLocals,
            finishCollectLocals = finishCollectLocals,
            warn = warn,
            signal = signal,
            leaf = leaf,
            call = collectUsageCall,
            handler = getCollectUsageHandler,
            globalenv = environment(fun),
            env = environment(fun),
            name = NULL,
            isLocal = collectUsageIsLocal)
}

collectUsageBioC <- function(fun, name = "<anonymous>", ...) {
    ## Imports
    collectUsageFun <- codetools:::collectUsageFun

    w <- makeUsageCollectorBioC(fun, ...)
    collectUsageFun(name, formals(fun), body(fun), w)
}

findGlobalsBioC <- function(fun, merge = TRUE) {
    ## Imports
    collectUsageFun <- codetools:::collectUsageFun
    isDDSym <- codetools:::isDDSym
    mkHash <- codetools:::mkHash

    enter <- function(type, v, e, w) {
        assign(v, value=TRUE, envir=if (type == "function") funs else vars)

        ## Handle '::' and ':::'
        if (is.element(v, c("::", ":::")) && typeof(e) == "language"){
            v <- deparse(e)
            assign(v, value=TRUE, envir=if (type == "function") funs_imports else vars_imports)
        }
    }
    leaf <- function(v, w) {
        if (typeof(v) == "symbol") {
            vn <- as.character(v)
            if (v == "...")
                w$signal("... may be used in an incorrect context", w)
            else if (isDDSym(v)) {
                if (w$isLocal("...", w))
                    w$enterLocal("variable", "...", v, w)
                else
                    w$signal(paste(v, "may be used in an incorrect context"), w)
            }
            else if (w$isLocal(vn, w))
                w$enterLocal("variable", vn, v, w)
            else if (! vn %in% c("*tmp*", "*tmpv*"))
                w$enterGlobal("variable", vn, v, w)
        } else if (typeof(v) == "closure" && w$followClosures) {
            w$env <- environment(v)
            collectUsageFun("<anonymous>", formals(v), body(v), w)
        }
    }

    vars <- mkHash()
    funs <- mkHash()
    vars_imports <- mkHash()
    funs_imports <- mkHash()

    collectUsageBioC(fun, followClosures = TRUE, enterGlobal = enter,
                     leaf = leaf)

    res <- list(
      functions = ls(funs, all.names = TRUE),
      variables = ls(vars, all.names = TRUE),
      functions_import = ls(funs_imports, all.names = TRUE),
      variables_import = ls(vars_imports, all.names = TRUE)
    )

    if (merge)
      res <- sort(unique(unlist(res, use.names=FALSE)))

    res
}
