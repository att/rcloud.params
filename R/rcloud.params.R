needed.params <- NULL
lookup <- function(name) {
  mget(name, envir=globalenv(), ifnotfound=NULL)
}
param <- function(var) {
  name <- deparse(substitute(var))
  val <- param.QS[[name]]
  if(!is.null(val)) {
    assign(name, val, envir=globalenv());
  } else {
    val = lookup(name)
  }
  callback <- function(val2) {
    assign(name, val2, envir=globalenv());
  }
  needed.params <- c(needed.params, name)
  param.caps$add_edit_control(Rserve.context(), paste0(name, ':&nbsp'), name,
                              val, rcloud.support:::make.oc(callback))
  invisible(TRUE)
}

is.done <- function() {
  return(TRUE)
  got <- mapply(needed.params, lookup)
  still.needed = got[is.null(got)]
  if(length(still.needed) > 0) {
    param.caps$focus(still.needed[[0]])
    FALSE
  } else {
    TRUE
  }
}

is.done.oc <- rcloud.support:::make.oc(is.done)

submit <- function() {
  param.caps$wait_submit(Rserve.context(), is.done.oc)
  invisible(TRUE)
}
