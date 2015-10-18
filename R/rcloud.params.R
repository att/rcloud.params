lookup <- function(name) {
  vals <- mget(name, envir=globalenv(), ifnotfound=NA)
  vals[[name]]
}
param <- function(var) {
  name <- deparse(substitute(var))
  def = lookup(name)
  if(is.na(def))
      def <- NULL
  val <- param.QS[[name]]
  if(!is.null(val)) {
    assign(name, val, envir=globalenv());
  }
  callback <- function(val2) {
    assign(name, val2, envir=globalenv());
  }
  param.caps$add_edit_control(Rserve.context(), paste0(name, ':&nbsp'), name,
                              def, val, rcloud.support:::make.oc(callback))
  invisible(TRUE)
}

is.done.oc <- rcloud.support:::make.oc(is.done)

submit <- function() {
  results <- param.caps$wait_submit(Rserve.context())
  mapply(function(name) {
    assign(name, results[[name]], globalenv())
  }, names(results))
  invisible(TRUE)
}
