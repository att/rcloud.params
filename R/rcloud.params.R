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

  defaultValue <- lookup(name)
  if(any(is.na(defaultValue))) 
    defaultValue <- NULL

  labelTag <- paste0('<label id="', paste0("rcloud-params-lab-", name),'">', paste0(label, ':&nbsp;') , '</label>')
  
  val <- input.QS[[name]] # Pull from query string if there ?
  
  if(!is.null(val[1])) {
    assign(name, val, envir=globalenv()); # If not in querySting assign to globalEnv
  }

  if(is.null(val) & !is.na(inputVal[1]))
    val <- inputVal    # If variable is undefined but user has set a value in widget, use this
  
  callback <- function(val2) {
    assign(name, val2, envir=globalenv());
  } # make call back ocap so variable created in js side can be assigned back to R

  
  ui.log.debug("Parameter:", paste0("Name: ", name, ", Default: ", defaultValue, ", Current: ", val, ", Class: ", varClass))
  widgetId <- input.caps$add_edit_control(Rserve.context(), paste0(name, ':&nbsp'), name,
                                          defaultValue, val, inputTag, labelTag, 
                                          varClass, rcloud.support:::make.oc(callback))
  
  ui.log.debug("HTML widget id: ", widgetId)

  return(structure(list( id = widgetId, name = name), class="rcloud.params.control"))
}

#' @export
print.rcloud.params.control <- function(x, ..., view = interactive()) {
  ui.log.debug("Printing widget: ", x$id)
  rcloud.html.out(as.character(x$control_tag, rcloud_htmlwidgets_print = FALSE))
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

#'
#'
#' @export
numericParam <- function(name, label = NULL, min = NA, max = NA, ...) {
  
  r_class <- "numeric"
  
  params_in <- list(...)
  
  ui.log.debug("Extra params: ", params_in)
  
  callbacks <- list()
  
  if('callbacks' %in% names(params_in)) {
    callbacks_in <- params_in[['callbacks']];
    for (callback in names(callbacks_in)) {
      if(!is.list(callbacks_in[callback])) {
        callbacks[callback] <- list(callbacks_in[callback])
      } else {
        callbacks[callback] <- callbacks_in[callback]
      }
    }
  }
  
  params_in['callbacks'] <- NULL
  
  ui.log.debug("Extra params: ", params_in)
  ui.log.debug("Callbacks: ", callbacks)
  
  label_id <- paste0("rcloud-params-lab-", name)
  input_id <- paste0("rcloud-params-input-", name)
  
  inputTag <- tag('input', c(list(type = 'number', min = min, max = max), params_in))
  inputTag$attribs$id <- input_id
  
  defaultValue <- lookup(name)
  if (any(is.na(defaultValue))) 
    defaultValue <- NULL
  
  # Set value
  if(is.null(inputTag$attribs$value)){
    inputTag$attribs$value <- ""
    tagValue <-  NULL
  } else{
    tagValue <- inputTag$attribs$value
  }
  
  qsValue <- input.QS[[name]] # Pull from query string if there ?
  
  if(!is.null(qsValue[1])) {
    assign(name, qsValue, envir=globalenv()); # If not in querySting assign to globalEnv
    value = qsValue
  } else if(!is.null(tagValue)) {
    value <- tagValue    # If variable is undefined but user has set a value in widget, use this
  } else {
    value <- defaultValue
  }
  
  if(!is.null(value) && !is.na(value)) {
    inputTag$attribs$value = value
  }
  
  assignValueCallback <- function(var_name, var_value, ...) {
    assign(var_name, var_value, envir=globalenv());
  } # make call back ocap so variable created in js side can be assigned back to R
  
  if('change' %in% names(callbacks)) {
    callbacks[['change']] <- append(list(assignValueCallback), callbacks[['change']])
  } else {
    callbacks[['change']] <- list(assignValueCallback)
  }
  
  ui.log.debug("Parameter:", paste0("Name: ", name, ", Default: ", defaultValue, ", Current: ", value, ", Class: ", r_class))
  controlTag <- tags$label(paste0(label, ': '), id=label_id, 'data-rcloud-param' = TRUE, 
                           'data-rcloud-param-name' = name, 'data-rcloud-htmlwidgets-inline' = TRUE, 'data-rcloud-param-rclass=' = r_class, 
                           'data-rcloud-param-default-value' = defaultValue, 'data-rcloud-param-value=' = value, 
                           inputTag)
  
  
  ui.log.debug("HTML widget id: ", label_id)
  
    
  control_descriptor <- structure(list(id = label_id, callbacks = callbacks, r_class=r_class, control_tag=controlTag), class="rcloud.params.control")
  
  assign(name, value = control_descriptor, envir = .params)
  
  return(controlTag)
}

