input.caps <- NULL
input.QS <- NULL


.onLoad <- function(libname, pkgname)
{
  f <- function(module.name, module.path) {
    path <- system.file("javascript", module.path, package="rcloud.params")
    caps <- rcloud.install.js.module(module.name,
                                     paste(readLines(path), collapse='\n')) 
    caps
  }
  input.caps <<- f("rcloud.params", "rcloud.params.js") 
  
  if(!is.null(input.caps)) {
    # init calls js queryString which grabs url and splits. init also has update method 
    input.QS <<- input.caps$init()  # $notebook in list with notebook id string
    
  }
  
}


.isDebugEnabled <- function() {
  OPT <- "rcloud.params.debug.enabled"
  if(rcloud.support:::nzConf(OPT)) {
    return(as.logical(rcloud.support:::getConf(OPT)))
  }
  return(FALSE)
}

ui.log.info <- function(...) {
  rcloud.params.ui.log.info(paste(..., collapse = ""));
}

ui.log.debug <- function(...) {
  if(.isDebugEnabled()) {
    rcloud.params.ui.log.debug(paste(..., collapse = ""));
  }
}

rcloud.params.ui.log.info <- function(content) input.caps$log(content)
rcloud.params.ui.log.debug <- function(content) input.caps$debug(content)