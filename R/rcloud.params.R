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
  print(x$content)
  
  if(x$wait_for) {
    waitForGroup()
  }
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

submitParam <- function(name = paste0("submit_", as.integer(runif(1)*1e6)), value = 'Submit', label = '', group = 'default', ...) {
  
  params_in <- list(...)
  
  ui.log.debug("Extra params: ", params_in)
  
  callbacks <- list()
  
  if('callbacks' %in% names(params_in)) {
    callbacks <- .processCallbackFunctions(params_in[['callbacks']])
  }
  
  params_in['callbacks'] <- NULL
  
  inputTag <- tags$button(value, id = name, type = 'submit', params_in, class="btn btn-default")
  
  control_descriptor <- .createControl(label, name, group, FALSE, FALSE, 'logical', inputTag, callbacks)
  
  return(control_descriptor$control_tag)
}

#' @export
waitForGroup <- function(group = 'default') {
  input.caps$wait_for_group(Rserve.context(), group)
}

#'
#' @export
dateParam <- function(name, label = NULL, group = 'default', ...) {
  
  r_class <- "Date"
  
  params_in <- list(...)
  
  ui.log.debug("Extra params: ", params_in)
  
  callbacks <- list()
  
  if('callbacks' %in% names(params_in)) {
    callbacks <- .processCallbackFunctions(params_in[['callbacks']])
  }
  
  params_in['callbacks'] <- NULL
  
  inputTag <- tag('input', c(type = 'date', params_in))
  
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
  
  if(inherits(value, "Date")){
    value <- as.character(value)
  }
  
  if(!is.null(value) && !is.na(value)) {
    inputTag$attribs$value = value
  }
  
  control_descriptor <- .createControl(label, name, group, defaultValue, value, r_class, inputTag, callbacks)
  
  assign(name, value = control_descriptor, envir = .params)
  
  return(control_descriptor$control_tag)
}

#'
#' @export
textParam <- function(name, label = NULL, group = 'default', ...) {
  
  r_class <- "character"
  
  params_in <- list(...)
  
  ui.log.debug("Extra params: ", params_in)
  
  callbacks <- list()
  
  if('callbacks' %in% names(params_in)) {
    callbacks <- .processCallbackFunctions(params_in[['callbacks']])
  }
  
  params_in['callbacks'] <- NULL
  
  inputTag <- tag('input', c(list(type = 'text'), params_in))
  
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
  
  control_descriptor <- .createControl(label, name, group, defaultValue, value, r_class, inputTag, callbacks)
  
  assign(name, value = control_descriptor, envir = .params)
  
  return(control_descriptor$control_tag)
}

numericSliderParam <- function(name, label = NULL, min = NA, max = NA, group = 'default', ...) {
  numericParam(name, label, min, max, group, type = 'range', class='slider', ...)
}

#'
#' @export
numericParam <- function(name, label = NULL, min = NA, max = NA, group = 'default', ...) {
  
  r_class <- "numeric"
  
  params_in <- list(...)
  
  ui.log.debug("Extra params: ", params_in)
  
  callbacks <- list()
  
  if('callbacks' %in% names(params_in)) {
    callbacks <- .processCallbackFunctions(params_in[['callbacks']])
  }
  
  params_in['callbacks'] <- NULL
  
  type <- 'number'
  
  if ('type' %in% names(params_in)) {
    type <- params_in$type
    params_in$type <- NULL
  }
  
  
  inputTag <- tag('input', c(list(type = type, min = min, max = max), params_in))
  
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
  
  control_descriptor <- .createControl(label, name, group, defaultValue, value, r_class, inputTag, callbacks)
  
  assign(name, value = control_descriptor, envir = .params)
  
  return(control_descriptor$control_tag)
}

selectParam <- function(name, label = NULL, choices = list(), group = 'default', ...) {
  
  r_class <- "character"
  
  params_in <- list(...)
  
  ui.log.debug("Extra params: ", params_in)
  
  callbacks <- list()
  
  if('callbacks' %in% names(params_in)) {
    callbacks <- .processCallbackFunctions(params_in[['callbacks']])
  }
  
  params_in['callbacks'] <- NULL
  
  
  inputTag <- tag('select', params_in)
  
  
  defaultValue <- lookup(name)
  if (any(is.na(defaultValue))) 
    defaultValue <- NULL
  
  # Set value
  if (is.null(inputTag$attribs$value)) {
    inputTag$attribs$value <- NULL
    tagValue <-  NULL
  } else {
    tagValue <- inputTag$attribs$value
    inputTag$attribs$value <- NULL
  }
  
  qsValue <- input.QS[[name]] # Pull from query string if there ?
  
  value <- NULL
  if (!is.null(qsValue[1])) {
    qsValues <- strsplit(qsValue[1], ',')[[1]]
    if (length(qsValues) > 0) {
      assign(name, qsValues, envir=globalenv()); # If in querySting assign to globalEnv
      value <- qsValues
    } else {
      # value set but it is empty
      value <- c()
    }
  } else if (!is.null(tagValue)) {
    value <- c(tagValue)    # If variable is undefined but user has set a value in widget, use this
  } else {
    value <- c(defaultValue)
  }
  
  ui.log.debug("Value: ", paste(value, collapse = ","))
  if (is.null(value) || is.na(value)) {
    value = c()
  }

  options <- NULL
  if (is.null(names(choices))) {
    options <- list(lapply(choices, function(c) {
      res <- tags$option(c)
      typedChoice <- as.character(c)
      if (typedChoice %in% value)
        res$attribs$selected = NA
      res
      }))
  } else {
    options <- list(lapply(names(choices), function(c) {
      res <- tags$option(choices[[c]], value=c)
      if (c %in% value)
        res$attribs$selected = NA
      res
    })
    )
  }
  
  inputTag$children <- options
  inputTag$attribs$choices <- NULL
  inputTag$attribs$value <- NULL
  
  control_descriptor <- .createControl(label, name, group, defaultValue, value, r_class, inputTag, callbacks)
  
  assign(name, value = control_descriptor, envir = .params)
  
  return(control_descriptor$control_tag)
}

checkboxParam <- function(name, label = NULL, group = 'default', ...) {
  
  r_class <- "logical"
  
  params_in <- list(...)
  
  ui.log.debug("Extra params: ", params_in)
  
  callbacks <- list()
  
  if('callbacks' %in% names(params_in)) {
    callbacks <- .processCallbackFunctions(params_in[['callbacks']])
  }
  
  params_in['callbacks'] <- NULL
  
  inputTag <- tags$input(type='checkbox', class="checkbox", params_in)
  
  defaultValue <- lookup(name)
  if (any(is.na(defaultValue))) 
    defaultValue <- NULL
  
  # Set value
  if (is.null(inputTag$attribs$checked)) {
    inputTag$attribs$checked <- NULL
    tagValue <-  NULL
  } else {
    tagValue <- if (is.logical(inputTag$attribs$checked)) {
      inputTag$attribs$checked
    } else {
      if(inputTag$attribs$checked == 'checked') {
        TRUE
      } else {
        FALSE
      }
    }
    inputTag$attribs$checked <- NULL
  }
  
  qsValue <- input.QS[[name]] # Pull from query string if there ?
  
  value <- FALSE
  if (!is.null(qsValue[1])) {
    qsValues <- as.logical(qsValue)
    assign(name, qsValues, envir=globalenv()); # If in querySting assign to globalEnv
    value <- qsValues
  } else if (!is.null(tagValue)) {
    value <- c(tagValue)    # If variable is undefined but user has set a value in widget, use this
  } else {
    value <- c(defaultValue)
  }
  
  ui.log.debug("Value: ", paste(value, collapse = ","))
  if (is.null(value) || is.na(value)) {
    value = FALSE
  }
  
  if(value) {
    inputTag$attribs$checked <- value
  }
  
  control_descriptor <- .createControl(label, name, group, defaultValue, value, r_class, inputTag, callbacks)
  
  assign(name, value = control_descriptor, envir = .params)
  
  return(control_descriptor$control_tag)
}

.controlLabelId <- function(name) {
  paste0("rcloud-params-lab-", name)
}

.controlInputId <- function(name) {
  paste0("rcloud-params-input-", name)
}
.controlId <- function(name) {
  paste0("rcloud-params-control-", name)
}

.rcloudParamsAttrNamespace <- function() {
  'data-rcloud-params'
}

.rcloudHtmlwidgetsInlineAttr <- function() {
  'data-rcloud-htmlwidgets-inline'
}

.rcloudParamsAttr <- function(name) {
  paste0(.rcloudParamsAttrNamespace(), "-", name)
}

.createControl <- function(label, name, group = 'default', default_value, param_value, r_class, input_tag, user_callbacks = list()) {
  label_id <- .controlLabelId(name)
  input_id <- .controlInputId(name)
  control_id <- .controlId(name)
  input_tag$attribs$id <- input_id
  
  varNameAttr <- .rcloudParamsAttr('name')
  
  input_tag$attribs[varNameAttr] <- name; 
  input_tag$attribs[.rcloudHtmlwidgetsInlineAttr()] <- TRUE;
  input_tag$attribs[.rcloudParamsAttr('group')] <- group;
  input_tag$attribs[.rcloudParamsAttr('rclass')] <- r_class;
  input_tag$attribs[.rcloudParamsAttr('default-value')] <- paste(default_value, collapse = ",");
  input_tag$attribs[.rcloudParamsAttr('value')] <- paste(param_value, collapse=",");
  
  if (is.null(input_tag$attribs$class)) {
    input_tag$attribs$class <- 'form-control'
  }
  
  labelMsg <- if(!is.null(label) && label != '') {
    paste0(label, ': ')
  } else {
    ''
  }
  
  ui.log.debug("Parameter - ", paste0("Name: ", name, ", Default: ", paste(default_value, collapse = ","), ", Current: ", paste(param_value, collapse=","), ", Class: ", r_class))

  # if('type' %in% names(input_tag$attribs) && input_tag$attribs$type == 'checkbox') {
  #   # just checkbox is a special case...
  #   input_tag$attribs$class <- NULL
  #   
  #   label_tag <- tags$label(labelMsg, 
  #                           id = label_id,
  #                           'for' = input_id)
  #   
  #   control_tag <- paramDiv(id = control_id, 
  #                           'class' = 'form-group', div(class='checkbox', label_tag, input_tag));
  # } else {
    label_tag <- tags$label(labelMsg, 
                            id = label_id,
                            'class' = 'control-label',
                            'for' = input_id)
    
    control_tag <- paramDiv(id = control_id, 
                            'class' = 'form-group', label_tag, input_tag)
  # }

  control_tag$attribs[.rcloudParamsAttrNamespace()] = TRUE
  control_tag$attribs[varNameAttr] = name
  
  assignValueCallback <- function(var_name, var_value, ...) {
    assign(var_name, var_value, envir=globalenv());
  } # make call back ocap so variable created in js side can be assigned back to R
  
  callbacks <- .prependCallbackFunction(user_callbacks, 'change', assignValueCallback)
  
  ui.log.debug("HTML widget id: ", control_id)
  
  structure(list(id = control_id, 
                 callbacks = callbacks, 
                 r_class = r_class, 
                 control_tag = control_tag),
            class="rcloud.params.control")
  
}

.processCallbackFunctions <- function(callbacksParam = list()) {
  callbacks <- list()
  for (callback in names(callbacksParam)) {
    if(!is.list(callbacksParam[callback])) {
      callbacks[callback] <- list(callbacksParam[callback])
    } else {
      callbacks[callback] <- callbacksParam[callback]
    }
  }
  invisible(callbacks)
}

.prependCallbackFunction <- function(callbacks = list(), eventType = 'change', FUN) {
  if(is.null(eventType) || is.null(FUN)) {
    callbacks
  } else {
    if(eventType %in% names(callbacks)) {
      callbacks[[eventType]] <- append(list(FUN), callbacks[[eventType]])
    } else {
      callbacks[[eventType]] <- list(FUN)
    }
    callbacks
  }
}