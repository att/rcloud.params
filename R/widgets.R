# get oject type to assign class back to R
getType <- function(`_tag_name`, varArgs) {
  if (`_tag_name` == "select") {
    "character"
  } else if (`_tag_name` == "input") {
    switch(varArgs$type,
           "text" = "character",
           "number" = "numeric",
           "date" = "Date",
           "range" = "numeric",
           "checkbox" = "logical",
           "character"
    )
  }
}

#' @export
mytags <- list(
  input = function(var, ...) ptag("input", substitute(var), list(...)), 
  select = function(var, ...) ptag("select",  substitute(var), list(...))
)

#' ptag
#'
#' @export
ptag <- function(`_tag_name`, var, varArgs){
  
  tag_out <- tag(`_tag_name`, varArgs)
  
  name <- deparse(var)
  
  if(is.null(tag_out$attribs$value)){
    tag_out$attribs$value <- ""
    value = NA
  } else{
    value <- tag_out$attribs$value
  }
  
  if(exists(name)){
    value <- eval(var)
    tag_out$attribs$value <- value
  }
  
  tag_out$attribs$id <- paste0("rcloud-params-", name)
  varClass <- getType(`_tag_name`, varArgs)
  
  param(inputTag = as.character(tag_out), name = name, 
        varClass = varClass, inputVal = value)  
  
}

