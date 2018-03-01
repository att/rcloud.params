lookup <- function(name) {
  vals <- mget(name, envir=globalenv(), ifnotfound=NA)
  vals[[name]]
}
textInput <- function(var){
  name <- deparse(substitute(var))
  inputTag <- paste0("<input type= 'text' id='", paste0("rcloud-params-", name),  "'></input>") 
  param(var, inputTag, name, varClass = "character")
}

selectInput <- function(var, choices, selected = var){
  name <- deparse(substitute(var))

  optionTag <- mapply(choices, choices, FUN = function(choice, label){
    sprintf('<option value="%s"%s>%s</option>',
            htmlEscape(choice, TRUE), 
            if (choice %in% selected) ' selected' else '', 
            htmlEscape(label))
  })
  optionTag <- HTML(paste(optionTag, collapse = '\n'))
  inputTag <- paste0("<select id='", paste0("rcloud-params-", name),  "'>", optionTag, "</select>")
  param(var, inputTag, name, varClass = "character")
  
}

dateInput <- function(var, value = var, format = "yyyy-mm-dd"){
  name <- deparse(substitute(var))
  #def <- as.character(format(def, "%d/%m/%Y"))
  if (inherits(value, "Date"))  value <- format(value, "%Y-%m-%d")
 
  inputTag <- '<input id="date" type="date" value="2017-06-01">'#paste0("<input type='date' id='", paste0("rcloud-params-", name), "' value = '", value,"'>")
  param(var, inputTag, name, varClass = "date")
}

numericInput <- function(var){
  name <- deparse(substitute(var))
  inputTag <- paste0("<input type= 'number' id='", paste0("rcloud-params-", name),  "'></input>") 
  param(var, inputTag, name, varClass = "numeric")
}

param <- function(var, inputTag, name, varClass) {

  def <- lookup(name)
  if(is.na(def)) 
    def <- NULL

  labelTag <- paste0('<label>', paste0(name, ':&nbsp') , '</label>')
  
  val <- input.QS[[name]] # Pull from query string if there ?
  if(!is.null(val)) {

    assign(name, val, envir=globalenv()); # If not in querySting assign to globalEnv
  }
  callback <- function(val2) {

    assign(name, val2, envir=globalenv());
  } # make call back ocap so variable created in js side can be assigned back to R
  
  # Should just re-use js to pass all inputs
  input.caps$add_edit_control(Rserve.context(), paste0(name, ':&nbsp'), name,
                            def, val, inputTag, labelTag, varClass, rcloud.support:::make.oc(callback))
  invisible(TRUE)
}



is.done.oc <- rcloud.support:::make.oc(is.done)

submit <- function(f = NULL) {
  results <- input.caps$wait_submit(Rserve.context())

    mapply(function(name) {
      #class(results[[name]]$val) <- results[[name]]$class
      assign(name, results[[name]], globalenv())
  }, names(results))
  
  if(!is.null(f)) {
    f()
  }
  
  invisible(TRUE)
}

# submitLocal <- function(f = NULL) {
#   results <- input.caps$wait_submit(Rserve.context())
#   
#   arg_env <- new.env()
#   
#   mapply(function(name) {
#     
#     assign(name, results[[name]], arg_env)
#   }, names(results))
#   
#   if(!is.null(f)) {
#     do.call(f, args = as.list(arg_env))
#   }
#   
#   invisible(TRUE)
# }


# submitEncaps <- function(f = NULL, env = parent.frame()) {
#   results <- input.caps$wait_submit(Rserve.context())
#   
#   mapply(function(name) {
#     
#     assign(name, results[[name]], env)
#   }, names(results))
#   
#   if(!is.null(f)) {
#     do.call(f, args = list(), envir = env)
#   }
#   
#   invisible(TRUE)
# }
# 
# 
# submitPlot <- function(x, y, data) {
#   results <- input.caps$wait_submit(Rserve.context())
#   
#   mapply(function(name) {
#     
#     assign(name, results[[name]], globalenv())
#   }, names(results))
#   
#   ggplot(aes_string(x, y), data = mtcars) +
#     geom_point()
#   #invisible(TRUE)
# }
