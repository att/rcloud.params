## These will be moved back to caps.R in rcloud.web
.html.in <- function(x){
  if (inherits(x, "javascript_function") || (is.character(x) && length(x) == 1)){ x }
  else {paste(as.character(x), collapse='\n')}
} 

#' @export
rcw.append <- function(element, what) {
  if (grepl("^rcloud-params-", what)) {
    input.caps$appendElement(element, what)
  } else{
    input.caps$appendDiv(element, .html.in(what))
  }
  
}
rcw.prepend <- function(element, what) {
  if (grepl("^rcloud-params-", what)) {
    input.caps$prependElement(element, what)
  } else{
    input.caps$prependDiv(element, .html.in(what))
  }
} 

#' @export
rcw.set <- function(element, what){
  if(grepl("^rcloud-params-",what)){

    input.caps$setElement(element, what)
  } else{
    input.caps$setDiv(element, .html.in(what))
  }
} 


