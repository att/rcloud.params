## These will be moved back to caps.R in rcloud.web
.html.in <- function(x){
  if (inherits(x, "javascript_function") || (is.character(x) && length(x) == 1)){ x }
  else {paste(as.character(x), collapse='\n')}
}


#' Append content to an HTML element
#'
#' @param selector target element selector, e.g. '#my-div'
#' @param what content to add, e.g. shiny.tag
#'
#' @return always TRUE
#' @export
rcloud.ui.append <- function(selector, what) {
  frontend$appendDiv(Rserve.context(), selector, .html.in(what))
}

#' Prepends content to an HTML element
#'
#' @param selector target element selector, e.g. '#my-div'
#' @param what content to prepend, e.g. shiny.tag
#'
#' @return always TRUE
#' @export
rcloud.ui.prepend <- function(selector, what) {
  frontend$appendDiv(Rserve.context(), selector, .html.in(what))
}

#' Sets content of an HTML element
#'
#' @param selector target element selector, e.g. '#my-div'
#' @param what content to set, e.g. shiny.tag
#'
#' @return TRUE
#' @export
rcloud.ui.set <- function(selector, what) {
  frontend$setDiv(Rserve.context(), selector, .html.in(what))
}

#' Generate plot in an HTML element
#'
#' @param selector target element selector, e.g. '#my-div'
#' @param plot.fun function producing a plot
#' @param width width of the plot
#' @param height height of the plot
#'
#' @return TRUE
#' @export
rcloud.ui.plot <- function(selector, plot.fun, width = 300, height = 300) {
  wp1 <- WebPlot(width = width, height = height)
  do.call(plot.fun, list(), envir = globalenv())
  frontend$setDiv(Rserve.context(), selector, .html.in(wp1))
}

#' Hide cell source
#' 
#' @export
rcloud.hide.source.cell <- function(cell.id) {
  frontend$hideCellSource(cell.id)
}

#' Hide currently executed cell's source
#' 
#' @export
rcloud.hide.source.current.cell <- function() {
  frontend$hideCurrentCellSource(Rserve.context())
}

#' @export
waitForForm <- function(form.id) {
  # Always stop the execution of next cells, and pass control to reactive UI.
  rcloud.stop.execution()
  frontend$waitForReactiveForm(Rserve.context(), form.id)
}

#' @export
validateForm <- function(form.id) {
  frontend$validateForm(Rserve.context(), form.id)
}
#'
#'  Blocks execution and waits for submission of a form associated with the given group
#' 
#' 
#' @export
waitForSynchronousForm <- function(form.id) {
  frontend$waitForForm(Rserve.context(), form.id)
}

#' Enable debug messages
#' 
#' @export
rcloud.params.debug.on <- function() {
  assign(DEBUG_OPTION, TRUE, envir = .options)
}

#' Disable debug messages
#' 
#' @export
rcloud.params.debug.off <- function() {
  assign(DEBUG_OPTION, FALSE, envir = .options)
}

ui.log.info <- function(...) {
  rcloud.params.ui.log.info(paste(..., collapse = ""));
}

ui.log.debug <- function(...) {
  if(.isDebugEnabled()) {
    rcloud.params.ui.log.debug(paste(..., collapse = ""));
  }
}

rcloud.params.ui.log.info <- function(content) frontend$log(content)
rcloud.params.ui.log.debug <- function(content) frontend$debug(content)