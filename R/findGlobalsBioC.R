makeUsageCollectorBioC <- function(fun, ..., name = NULL,
        enterLocal = codetools:::doNothing,
        enterGlobal = codetools:::doNothing,
        enterInternal = codetools:::doNothing,
        startCollectLocals = codetools:::doNothing,
        finishCollectLocals = codetools:::doNothing,
        warn = codetools:::warning0,
        signal = codetools:::signalUsageIssue,
        leaf = codetools:::collectUsageLeaf) {
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
            call = codetools:::collectUsageCall,
            handler = codetools:::getCollectUsageHandler,
            globalenv = environment(fun),
            env = environment(fun),
            name = NULL,
            isLocal = codetools:::collectUsageIsLocal)
}

collectUsageBioC <- function(fun, name = "<anonymous>", ...) {
    w <- makeUsageCollectorBioC(fun, ...)
    codetools:::collectUsageFun(name, formals(fun), body(fun), w)
}

findGlobalsBioC <- function(fun, merge = TRUE) {
    enter <- function(type, v, e, w)
        assign(v, TRUE, if (type == "function") funs else vars)
    leaf <- function(v, w) {
        if (typeof(v) == "symbol") {
            vn <- as.character(v)
            if (v == "...")
                w$signal("... may be used in an incorrect context", w)
            else if (codetools:::isDDSym(v)) {
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
            codetools:::collectUsageFun("<anonymous>", formals(v), body(v), w)
        }
    }
    vars <- codetools:::mkHash()
    funs <- codetools:::mkHash()

    collectUsageBioC(fun, followClosures = TRUE, enterGlobal = enter,
                     leaf = leaf)
    fnames <- ls(funs, all.names = TRUE)
    vnames <- ls(vars, all.names = TRUE)
    if (merge)
        suniquec(vnames, fnames)
    else list(functions = fnames, variables = vnames)
}
