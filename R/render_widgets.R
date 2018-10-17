# get oject type to assign class back to R
getType <- function(param) {
  if (param$input == "select") {
    "character"
  } else if (param$input == "input") {
    switch(param$type,
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
  
  ui.log.debug("Param set elements:", paste0(names(params_in), collapse = ","))
  
  result <- list();
  
  for(i in names(params_in)) {
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
      
      if(is.function(value)){
        stop(paste0("The variable name you have selected (", name,") is invalid, please choose another"))
      }
 
      tag_out$attribs$value <- value
    }
    
    tag_out$attribs$id <- paste0("rcloud-params-", name)

    if(param$input == "select"){
      
      tag_out$children <- list(lapply(param$choices, tags$option))
      tag_out$attribs$choices <- NULL
      tag_out$attribs$value <- NULL
    }

    if(param$type == "checkbox" && !is.na(value)){
      if(value == TRUE){
        tag_out$attribs$checked <- "checked"
      }
    }
    
    if(inherits(value, "Date")){
      value <- as.character(value)
    }

    varClass <- getType(param)
    label <- ifelse(is.null(param$label), name, param$label)

    control <- param(inputTag = as.character(tag_out), name = name,
                     varClass = varClass, inputVal = value, label = label);
    if(!inherits(control, 'rcloud.params.control')) {
      stop("unexpected type of control " + class(control))
    }
    result[i] <- list(control)
  }
  
  return(structure(list(params = result), class="rcloud.params.param.set"));
}

