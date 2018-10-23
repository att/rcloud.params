input.caps <- NULL
input.QS <- NULL

.params <- new.env();


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
    ocaps <- list(
      handle_event = rcloud.support:::make.oc(function(var_name, var_value, e) {
          ui.log.debug("Event received", var_name, var_value, paste0(as.character(e), collapse = ""))
          ui.log.debug("Event type", e$type)
          if(!is.null(var_name)) {
            if(var_name %in% names(.params)) {
              control <- get(var_name, .params);
              if(!is.null(control$callbacks[e$type])) {
                lapply(control$callbacks[[e$type]], function(fun) {
                  ui.log.debug(deparse(fun))
                  fun(var_name, var_value, e)
                })
              }
            }
          }
          invisible(TRUE)
        }));
    
    # init calls js queryString which grabs url and splits. init also has update method 
    input.QS <<- input.caps$init(ocaps)  # $notebook in list with notebook id string
    
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