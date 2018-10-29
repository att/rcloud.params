#' Create div that will include parms widgets
#' @description  Allows users to enter param widgets with html tools all in one call
#' @param byRow alter widget display (T/F)
#' @examples 
#'    div(h1("Start"),h1("Start"),
#'        mytags$input(v, val = 2, type = "number"),
#'        mytags$input(z, type = "date"),
#'        h1("End"))
#'    submit()
#' @export

##  This function uses the rcw.* functions from rcloud.web. 
## These functions have been copied to rcloud.params.js but will need to be updated 
## when the package is transfered.

#' @export
paramDiv <- function(...) {
  divTag <- div(...)
  divTag$attribs[.rcloudHtmlwidgetsInlineAttr()] <- TRUE
  return(divTag)
}

#' @export
param_set2 <- function(..., callbacks = list(), wait_for = FALSE, name = paste0("submit_", as.integer(runif(1)*1e6))) {
  
  in_params <- list(...)
  
  if (length(in_params) == 0) {
    stop('No parameters were provided!');
  }
  
  content = tags$form(name = name, in_params)
  content$attribs[.rcloudHtmlwidgetsInlineAttr()] <- TRUE
  
  return(structure(list(content = content, callbacks = callbacks, wait_for = wait_for), class="rcloud.params.param.set"));
}