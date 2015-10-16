param <- function(var) {
  name <- deparse(substitute(var))
  val <- param.QS[[name]]
  if(!is.null(val)) {
    assign(name, val, envir=globalenv());
  }
  param.caps$add_edit_control(Rserve.context(), paste0(name, ':&nbsp'), val)
}
