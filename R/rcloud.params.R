#' Lookup variable name in globalenv
#'
lookup <- function(name) {
  vals <- mget(name, envir=globalenv(), ifnotfound=NA)
  vals[[name]]
}

#' Pass parameters to javascript
#'
#' Takes HTML string and passes to javascript. Variables can be updated through widget then passed back to R
#'
#' @param inputTag HTML string to create widget
#' @param name varibale name
#' @param varClass class of variable 

param <- function(inputTag, name, varClass, inputVal = NA, label = "") {

  def <- lookup(name)
  if(any(is.na(def))) 
    def <- NULL

  labelTag <- paste0('<label id="', paste0("rcloud-params-lab-", name),'">', paste0(label, ':&nbsp') , '</label>')
  
  val <- input.QS[[name]] # Pull from query string if there ?
  
  if(!is.null(val[1])) {
    assign(name, val, envir=globalenv()); # If not in querySting assign to globalEnv
  }
  if(is.null(val) & !is.na(inputVal[1]))
    val <- inputVal    # If variable is undefined but user has set a value in widget, use this
  
  callback <- function(val2) {
    assign(name, val2, envir=globalenv());
  } # make call back ocap so variable created in js side can be assigned back to R


  widgetId <- input.caps$add_edit_control(Rserve.context(), paste0(name, ':&nbsp'), name,
                            def, val, inputTag, labelTag, varClass, rcloud.support:::make.oc(callback))
  
  ui.log.debug("HTML widget id: ", widgetId)

  return(structure(list( id = widgetId, name = name), class="rcloud.params.control"))
}

#' @export
print.rcloud.params.control <- function(x, ..., view = interactive()) {
  rcw.append("", x)
}

#' @export
print.rcloud.params.param.set <- function(x, ..., view = interactive()) {
  invisible(lapply(x$params, function(param) {
    print(param)
  }))
}


#' Returns output of widget to R side when clicked
#'
#' @param f optional. Run a user defined function
#' @export
submit <- function(f = NULL) {
  results <- input.caps$wait_submit(Rserve.context())

    mapply(function(name) {
      if(results[[name]]$type == "Date") 
        results[[name]]$value <- as.Date(results[[name]]$value)

      class(results[[name]]$value) <- results[[name]]$type
      assign(name, results[[name]]$value, globalenv())
     
  }, names(results))
  
  if(!is.null(f)) {
    f()
  }
  
  invisible("rcloud-params-submit")
}

