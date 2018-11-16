frontend <- NULL

.params <- new.env()
.options <- new.env()
.query.params <- new.env()

DEBUG_OPTION <- 'DEBUG'
QUERY_PARAMS <- 'QUERY_PARAMS'

.onLoad <- function(libname, pkgname)
{
  f <- function(module.name, module.path) {
    path <- system.file("javascript", module.path, package="rcloud.params")
    caps <- rcloud.install.js.module(module.name,
                                     paste(readLines(path), collapse='\n')) 
    caps
  }
  frontend <<- f("rcloud.params", "rcloud.params.js") 
  
  if(!is.null(frontend)) {
    ocaps <- list(
      handle_event = rcloud.support:::make.oc(dispatchEvent), 
      set_qs_params = rcloud.support:::make.oc(setQSParams));
    
    setQSParams(frontend$init(ocaps))
  }
  
  assign(DEBUG_OPTION, .isDebugEnabledInRCloudConfig(), envir = .options)
}

.isDebugEnabledInRCloudConfig <- function() {
  OPT <- "rcloud.params.debug.enabled"
  if(rcloud.support:::nzConf(OPT)) {
    return(as.logical(rcloud.support:::getConf(OPT)))
  }
  return(FALSE)
}

.isDebugEnabled <- function() {
  return(get(DEBUG_OPTION, envir = .options))
}

