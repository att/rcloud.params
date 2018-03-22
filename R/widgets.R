#' Create text input
#'
#' Creates HTML for a textbox input to pass to the param function
#' @export

textInput <- function(var){
  name <- deparse(substitute(var))
  inputTag <- paste0("<input type= 'text' id='", paste0("rcloud-params-", name),  "'></input>") 
  #inputTag <- as.character(tags$input(type = "text", id = paste0("rcloud-params-", name)))
  param(inputTag, name, varClass = "character")
}

#' Create date input
#'
#' Creates HTML for a select dropdown input to pass to the param function
#' @export

selectInput <- function(var, choices, selected = ''){
  name <- deparse(substitute(var))
  
  optionTag <- mapply(choices, choices, FUN = function(choice, label){
    sprintf('<option value="%s"%s>%s</option>',
            htmlEscape(choice, TRUE), 
            if (choice %in% selected) ' selected' else '', 
            htmlEscape(label))
  })
  optionTag <- HTML(paste(optionTag, collapse = '\n'))
  inputTag <- paste0("<select id='", paste0("rcloud-params-", name),  "'>", optionTag, "</select>")
  param(inputTag, name, varClass = "character")
  
}

#' Create date input
#'
#' Creates HTML for a clickable calender input that is passed to the param function
#' @export

dateInput <- function(var, value = var, format = "yyyy-mm-dd"){
  name <- deparse(substitute(var))

  #if (inherits(value, "Date"))  value <- format(value, "%Y-%m-%d")
  
  inputTag <- paste0("<input type='date' id='", paste0("rcloud-params-", name),
                     "' value = '", as.character(value), "' data-date-format='yyyy-mm-dd' >")
 
  param(inputTag, name, varClass = "Date")
}

#' Create date input
#'
#' Creates HTML for a numeric value input to pass to the param function
#' @export

numericInput <- function(var){
  name <- deparse(substitute(var))
  inputTag <- paste0("<input type= 'number' id='", paste0("rcloud-params-", name),  "'></input>") 
  param(inputTag, name, varClass = "numeric")
}

sliderInput <- function(var, min = 0, max = 10, value = 5){
  name <- deparse(substitute(var))
  inputTag <- paste0("<input type= 'range' id='", paste0("rcloud-params-", name),  "' min ='",
                     min, "' max = '", max, "' value = '", value, "'></input>")
  
  param(inputTag, name, varClass = "numeric")
}
