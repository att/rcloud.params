
#' Dispatch event handling to appropriate control's callback function
#' 
dispatchEvent <- function(par.name, var.value, e) {
  .ui.log.debug("Event received", e$type, par.name, var.value, paste0(as.character(e), collapse = ""))
  if (!is.null(par.name)) {
    if (par.name %in% names(.params)) {
      control <- get(par.name, .params)
      typedValue <- control$uiToRValueMapper(var.value)
      if (!is.null(control$callbacks) && !is.null(control$callbacks[[e$type]])) {
        lapply(control$callbacks[[e$type]], function(fun) {
          if (is.function(fun)) {
            .ui.log.debug(deparse(fun))
            do.call(fun, list(control$varName, typedValue, e))
          }
        })
      }
    }
  }
  invisible(TRUE)
}

setQSParams <- function(params) {
  assign(QUERY_PARAMS, params, envir = .query.params)  # $notebook in list with notebook id string
}