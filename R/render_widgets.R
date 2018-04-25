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

#' fixInputType
#' @description sets the 'type' to either select or specified input eg. text. 
#'              'input' is either select OR input 
fixInputType <- function(param){
  if(param$input == 'select'){
    param$type <- param$input
    param$input <- "select"
  } else {
    # for an input type must be set
    param$type <- param$input
    param$input <- "input"
  }
  if(param$type == "numeric") param$type <- "number"
  if(param$type == "slider") param$type <- "range"
  
  param
}

#' params_set
#'
#' @examples param_set(minimum = list(label = "Minimum", value = 10, min = 1, input = "number"),
#'                     maximum = list(label = "Maximum", value = 20, min = 1, input = "number"))
#' @export

param_set <- function(...){
  params_in  <- list(...)
  
  for(i in names(params_in)){
    param <- params_in[[i]]
    name <- i
    param <- fixInputType(param) 
    tag_out <- tag(param$input, param) 
    
    #Set value
    if(is.null(tag_out$attribs$value)){
      tag_out$attribs$value <- ""
      value <-  NA
    } else{
      value <- tag_out$attribs$value
    }
    
    if(exists(name)){
      value <- get(name)
      tag_out$attribs$value <- value
    }
    
    tag_out$attribs$id <- paste0("rcloud-params-", name)
    
    if(param$input == "select"){
      tag_out$children <- list(lapply(param$choices, tags$option))
    }
    
    varClass <- getType(param$input, param)
    label <- ifelse(is.null(param$label), name, param$label)
    
    param(inputTag = as.character(tag_out), name = name,
          varClass = varClass, inputVal = value, label = label)
  }
}


#' param_add
#'
#' @export
param_add <- function(var, varArgs){
  
tagName <- ifelse(varArgs$type == "select", "select", "input")
  
  tag_out <- tag(tagName, varArgs)
  
  name <- deparse(substitute(var))
  
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
  label <- ifelse(is.null(tag_out$attribs$label), name, tag_out$attribs$label)
  varClass <- getType(tagName, varArgs)
  
  param(inputTag = as.character(tag_out), name = name, 
        varClass = varClass, inputVal = value, label = label)  
  
}

