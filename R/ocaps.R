
#' Dispatch event handling to appropriate control's callback function
#' 
dispatchEvent <- function(par_name, var_value, e) {
  ui.log.debug("Event received", e$type, par_name, var_value, paste0(as.character(e), collapse = ""))
  if (!is.null(par_name)) {
    if (par_name %in% names(.params)) {
      control <- get(par_name, .params)
      typed_value <- control$uiToRValueMapper(var_value)
      if (!is.null(control$callbacks) && !is.null(control$callbacks[[e$type]])) {
        lapply(control$callbacks[[e$type]], function(fun) {
          if (is.function(fun)) {
            ui.log.debug(deparse(fun))
            do.call(fun, list(control$var_name, typed_value, e))
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