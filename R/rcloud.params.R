#' lookup variable value in globalenv
#'
.getVariableValue <- function(name) {
  vals <- mget(name, envir=globalenv(), ifnotfound=NA)
  val <- vals[[name]]
  if (any(is.na(val))) 
    val <- NULL
  val
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
#' Creates date-picker input control
#' 
#' @export
#' 
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
  
  defaultValue <- .getVariableValue(name)
  
  tagValue <- .getValueFromTagAttribute(inputTag)
  
  qsValue <- .getSingleValueFromQueryParameter(name, r_class)
  
  if(!is.null(qsValue)) {
    value <- qsValue
  } else if(!is.null(tagValue)) {
    value <- tagValue    # If variable is undefined but user has set a value in widget, use this
  } else {
    value <- defaultValue
  }
  
  if(is.null(value) || is.na(value)) {
    value = ''
  }
  
  if (length(value) > 0) {
    assign(name, value, envir=globalenv())
  }
  
  if(inherits(value, "Date")){
    value <- as.character(value)
  }
  
  inputTag <- .rToUIControlValueMapper('input')(inputTag, value)
  
  control_descriptor <- .createControl(label, name, group, defaultValue, value, r_class, inputTag, callbacks)
  
  assign(name, value = control_descriptor, envir = .params)
  
  return(control_descriptor$control_tag)
}

#' 
#' Creates text input control
#' 
#' @export
#' 
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
  
  defaultValue <- .getVariableValue(name)
  
  tagValue <- .getValueFromTagAttribute(inputTag)
  
  qsValue <- .getSingleValueFromQueryParameter(name, r_class)
  
  if(!is.null(qsValue)) {
    value <- qsValue
  } else if(!is.null(tagValue)) {
    value <- tagValue    # If variable is undefined but user has set a value in widget, use this
  } else {
    value <- defaultValue
  }
  
  if(is.null(value) || is.na(value)) {
    value = ''
  }
  
  if (length(value) > 0) {
    assign(name, value, envir=globalenv())
  }

  inputTag <- .rToUIControlValueMapper('input')(inputTag, value)
  
  control_descriptor <- .createControl(label, name, group, defaultValue, value, r_class, inputTag, callbacks)
  
  assign(name, value = control_descriptor, envir = .params)
  
  return(control_descriptor$control_tag)
}

#' 
#' Creates numeric slider input control
#' 
#' @export
#' 
numericSliderParam <- function(name, label = NULL, min = NA, max = NA, group = 'default', ...) {
  numericParam(name, label, min, max, group, type = 'range', class='form-control slider', ...)
}

#' 
#' Creates numeric input control
#' 
#' @export
#' 
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
  
  defaultValue <- .getVariableValue(name)
  
  tagValue <- .getValueFromTagAttribute(inputTag)
  
  qsValue <- .getSingleValueFromQueryParameter(name, r_class)
  
  if(!is.null(qsValue)) {
    value <- qsValue
  } else if(!is.null(tagValue)) {
    value <- tagValue    # If variable is undefined but user has set a value in widget, use this
  } else {
    value <- defaultValue
  }
  
  if(is.null(value) || is.na(value)) {
    value <- ''
  }
  
  if (length(value) > 0) {
    assign(name, value, envir=globalenv())
  }
  
  inputTag <- .rToUIControlValueMapper('input')(inputTag, value)
  
  control_descriptor <- .createControl(label, name, group, defaultValue, value, r_class, inputTag, callbacks)
  
  assign(name, value = control_descriptor, envir = .params)
  
  return(control_descriptor$control_tag)
}

#' 
#' Creates select input control
#' 
#' @export
#' 
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
  
  defaultValue <- .getVariableValue(name)
  
  tagValue <- .getValueFromTagAttribute(inputTag)
  
  qsValue <- .getMultiValueFromQueryParameter(name, r_class) 
  
  value <- NULL
  if (!is.null(qsValue)) {
    value <- qsValue
  } else if (!is.null(tagValue)) {
    value <- tagValue    # If variable is undefined but user has set a value in widget, use this
  } else {
    value <- defaultValue
  }
  
  if (is.null(value) || any(is.na(value))) {
    value = c()
  }
  
  if (length(value) == 0 && (!'multiple' %in% names(inputTag$attribs)) && length(choices) > 0) {
    value <- if (is.null(names(choices))) {
      choices[1]
    } else {
      names(choices)[1]
    }
  }
  
  if (length(value) > 0) {
    assign(name, value, envir=globalenv())
  }

  inputTag <- .rToUIControlValueMapper('select')(inputTag, value, choices)
  
  control_descriptor <- .createControl(label, name, group, defaultValue, value, r_class, inputTag, callbacks)
  
  assign(name, value = control_descriptor, envir = .params)
  
  return(control_descriptor$control_tag)
}

#' 
#' Creates checkbox input control
#' 
#' @export
#' 
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
  
  defaultValue <- .getVariableValue(name)
  
  tagValue <- NULL
  if('checked' %in% names(inputTag$attribs)) {
    if (!is.null(inputTag$attribs$checked) && !any(is.na(inputTag$attribs$checked))) {
      tagValue <- if (is.logical(inputTag$attribs$checked)) {
        inputTag$attribs$checked
      } else {
        if(inputTag$attribs$checked == 'checked') {
          TRUE
        } else {
          FALSE
        }
      }
    }
  }
  
  qsValue <- .getSingleValueFromQueryParameter(name, r_class)
  
  if(!is.null(qsValue)) {
    value <- qsValue
  } else if (!is.null(tagValue)) {
    value <- tagValue    # If variable is undefined but user has set a value in widget, use this
  } else {
    value <- defaultValue
  }
  
  ui.log.debug("Value: ", paste(value, collapse = ","))
  
  if (is.null(value) || is.na(value)) {
    value <- FALSE
  }
  
  if (length(value) > 0) {
    assign(name, value, envir=globalenv())
  }

  inputTag <- .rToUIControlValueMapper('checkbox')(inputTag, value)
  
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
  
  labelMsg <- label
  
  ui.log.debug("Parameter - ", paste0("Name: ", name, ", Default: ", paste(default_value, collapse = ","), ", Current: ", paste(param_value, collapse=","), ", Class: ", r_class))

  if('type' %in% names(input_tag$attribs) && input_tag$attribs$type == 'checkbox') {
    # just checkbox is a special case...
    input_tag$attribs$class <- NULL

    label_tag <- tags$label(labelMsg,
                            id = label_id,
                            'for' = input_id)

    control_tag <- paramDiv(id = control_id,
                            'class' = 'form-group', div(class='checkbox', label_tag, input_tag));
  } else {
    label_tag <- tags$label(labelMsg, 
                            id = label_id,
                            'class' = 'control-label',
                            'for' = input_id)
    
    control_tag <- paramDiv(id = control_id, 
                            'class' = 'form-group', label_tag, input_tag)
  }

  control_tag$attribs[.rcloudParamsAttrNamespace()] = TRUE
  control_tag$attribs[varNameAttr] = name
  
  assignValueCallback <- function(var_name, var_value, ...) {
    ui.log.debug(var_name, paste0(var_value, collapse = ","), typeof(var_value))
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


.getMultiValueFromQueryParameter <- function(name, r_class) {
  qsValue <- input.QS[[name]]
  value <- NULL
  if (!is.null(qsValue[1])) {
    valueMapper <- .uiToRValueMapper(r_class)
    parser <- .qsMultiValueParser()
    value <- valueMapper(parser(qsValue))
  }
  value
}

.getSingleValueFromQueryParameter <- function(name, r_class) {
  qsValue <- input.QS[[name]]
  value <- NULL
  if (!is.null(qsValue[1])) {
    valueMapper <- .uiToRValueMapper(r_class)
    parser <- .qsSingleValueParser()
    value <- valueMapper(parser(qsValue))
  }
  value
}

.qsMultiValueParser <- function(value_type) {
  function(value) {
           values <- strsplit(value[1], ',')[[1]]
           if (length(values) > 0) {
             values
           } else {
             c()
           }
         }
}

.qsSingleValueParser <- function() {
  function(value) {
     value
  }
}

.uiToRValueMapper <- function(r_class) {
  switch(r_class, 
         'character' = as.character,
         'Date' = as.Date,
         'numeric' = as.numeric,
         'logical' = as.logical,
         as.character)
}

.rToUIControlValueMapper <- function(control_type) {
  switch(control_type, 
         'select' = function(tag, value, choices) {
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
           
           tag$children <- options
           tag$attribs$choices <- NULL
           tag$attribs$value <- NULL
           tag
         },
         'checkbox' = function(tag, value) {
           tag$attribs$value <- NULL
           if (value) {
             tag$attribs$checked <- value
           } else {
             tag$attribs$checked <- NULL
           }
           tag
         },
         function(tag, value) {
           tag$attribs$value <- as.character(value)
           tag
         })
}

.getValueFromTagAttribute <- function(inputTag) {
  tagValue <- NULL
  if('value' %in% names(inputTag$attribs)) {
    if (!is.null(inputTag$attribs$value) && !any(is.na(inputTag$attribs$value))) {
      tagValue <- inputTag$attribs$value
    }
  }
  tagValue
}

.processCallbackFunctions <- function(callbacksParam = list()) {
  callbacks <- list()
  for (callback in names(callbacksParam)) {
    if (!is.list(callbacksParam[callback])) {
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