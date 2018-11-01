
#' Dispatch event handling to appropriate control's callback function
#' 
dispatchEvent <- function(var_name, var_value, e) {
  ui.log.debug("Event received", e$type, var_name, var_value, paste0(as.character(e), collapse = ""))
  if(!is.null(var_name)) {
    if(var_name %in% names(.params)) {
      control <- get(var_name, .params)
      typed_value <- control$uiToRValueMapper(var_value)
      if(!is.null(control$callbacks[e$type])) {
        lapply(control$callbacks[[e$type]], function(fun) {
          ui.log.debug(deparse(fun))
          fun(var_name, typed_value, e)
        })
      }
    }
  }
  invisible(TRUE)
}