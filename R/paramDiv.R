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


.runWidgetCode <- function(code) {
  
  output <- tryCatch({ 
    tres <- eval(parse(text = code))
    if(inherits(tres, 'shiny.tag')) {
      return(list(tres))
    }
    tres
  }, error = function(e) {
    return(structure(list("msg", paste0("Failed to generate widget: ", as.character(e) )), class="code-eval-error"))
  })
  
  ui.log.debug("Code:", code, ". Output: ", paste0(output, collapse = ","), " of type ", class(output))
  if(inherits(output, "code-eval-error")) {
    output <- list(paste0('<span class="error">Widget error: ', output$msg, '</span>'))
  }
  return(output)
}

paramDiv <- function(...) {
  divTag <- div(...)
  divTag$attribs['data-rcloud-htmlwidgets-inline'] <- TRUE
  return(divTag)
}