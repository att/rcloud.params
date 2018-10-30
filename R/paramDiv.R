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


#' 
#' 
#' Produced a div with additional attribute ('data-rcloud-htmlwidgets-inline') 
#' 
#' The resulting div will not be wrapped by iframe by rcloud.htmlwidgets.
#' 
#' @param ... htmltools div function parameters
#' @return shiny.tag 
#'  
#' @export
paramDiv <- function(...) {
  divTag <- div(...)
  divTag$attribs[.rcloudHtmlwidgetsInlineAttr()] <- TRUE
  return(divTag)
}


#' Prints a param set form
#'
#' @export
print.rcloud.params.param.set <- function(x, ..., view = interactive()) {
  print(x$content)
  
  if(x$wait_for) {
    controlValues <- waitForGroup()
    
    if(!is.null(controlValues)) {
      lapply(controlValues, function(el) {
        r_class <- if(is.null(el$r_class)) {
          'character'
        } else {
          el$r_class
        }
        assign(el$name, .uiToRValueMapper(r_class)(el$value))
      });
    }
  }
  invisible(TRUE)
}

#' Creates param set form
#' 
#' Note! paramSet is not a shiny.tag, this means that it may not be wrapped in htmltools shiny.tag
#' 
#' @param ... child elements (shiny.tags)
#' @param callbacks form callbacks
#' @param wait_for should the R process be blocked when form is displayed
#' @param name name of the form
#' @param group the group of the controls
#' 
#' @return rcloud.params.param.set structure 
#' 
#' @export 
paramSet <- function(..., callbacks = list(), wait_for = FALSE, name = paste0("form_", as.integer(runif(1)*1e6)), group = 'default') {
  
  in_params <- list(...)
  
  if (length(in_params) == 0) {
    stop('No parameters were provided!')
  }
  
  content = tags$form(name = name, in_params)
  content$attribs[.rcloudHtmlwidgetsInlineAttr()] <- TRUE
  content$attribs[.rcloudParamsAttrNamespace()] <- TRUE
  content$attribs[.rcloudParamsAttr('group')] <- group;
  
  param_set_descriptor <- structure(list(name = name, content = content, callbacks = callbacks, wait_for = wait_for), class="rcloud.params.param.set")
  
  .registerControl(param_set_descriptor)
  
  return(param_set_descriptor)
}