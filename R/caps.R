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

.ui.log.info <- function(...) {
  .rcloud.params.ui.log.info(paste(..., collapse = ""));
}

.ui.log.debug <- function(...) {
  if(.isDebugEnabled()) {
    .rcloud.params.ui.log.debug(paste(..., collapse = ""));
  }
}
.rcloud.params.ui.log.info <- function(content) frontend$log(content)
.rcloud.params.ui.log.debug <- function(content) frontend$debug(content)