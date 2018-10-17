## These will be moved back to caps.R in rcloud.web
.html.in <- function(x){
  if (inherits(x, "javascript_function") || (is.character(x) && length(x) == 1)){ x }
  else {paste(as.character(x), collapse='\n')}
} 

#' @export
rcw.append <- function(element, what) {
  if (inherits( what, 'rcloud.params.control')) {
    input.caps$appendElement(Rserve.context(), element, what)
  } else{
    input.caps$appendDiv(Rserve.context(), element, .html.in(what))
  }
  
}
rcw.prepend <- function(element, what) {
  if (inherits( what, 'rcloud.params.control')) {
    input.caps$prependElement(element, what)
  } else{
    input.caps$prependDiv(element, .html.in(what))
  }
} 

#' @export
rcw.set <- function(element, what){
  if (inherits( what, 'rcloud.params.control')) {
    input.caps$setElement(Rserve.context(), element, what)
  } else{
    input.caps$setDiv(Rserve.context(), element, .html.in(what))
  }
} 


