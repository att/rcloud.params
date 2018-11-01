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
#' @param plot_fun function producing a plot
#' @param width width of the plot
#' @param height height of the plot
#'
#' @return TRUE
#' @export
rcloud.ui.plot <- function(selector, plot_fun, width = 300, height = 300) {
  wp1 <- WebPlot(width = width, height = height)
  do.call(plot_fun, list(), envir = globalenv())
  frontend$setDiv(Rserve.context(), selector, .html.in(wp1))
}

#' Run cell with given id
#' 
#' @export
rcloud.run.cell <- function(cell_id) {
  frontend$run_cell(cell_id)
}

#' Run cells with given ids
#' 
#' @export
rcloud.run.cells <- function(cell_ids) {
  frontend$run_cells(cell_ids)
}

#' Run all cells starting from the given cell id
#' 
#' @export
rcloud.run.cells.from <- function(cell_id) {
  frontend$run_cells_from(cell_id)
}

#'
#'  Blocks execution and waits for submission of a form associated with the given group
#' 
#' TODO Consider removing this
#' 
#' @export
waitForGroup <- function(group = 'default') {
  frontend$wait_for_group(Rserve.context(), group)
}

#' Gracefully stops execution of a notebook
#' 
#' @export
rcloud.stop.execution <- function() {
  frontend$stop_execution()
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