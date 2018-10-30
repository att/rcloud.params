#'
#' Factory creating rcloud.params controls
#' 
#' 
#' The factory is configurable via a number of strategies that address variability in the process of creating shiny.tags representing rcloud.params controls.
#' 
#' The returned value is a shiny.tag structure, the associated rcloud.params.control is persisted in the packages environment and is used to invoke control-specific callback functions.
#' 
#' @param name the name of the variable
#' @param label the label for the control
#' @param r_class type of the variable
#' @param group the group the control belongs to
#' @param tagFactory the function returning form input shiny.tags
#' @param tagValueExtractor a function that extracts value specified via shiny.tag attributes
#' @param qsValueExtractor a function that extracts value from a query string
#' @param nullValueProvider a function that returns a value that should be used if default, query string nor shiny.tag specify a value
#' @param rToTagValueMapper a mapper that converts R value to a value accepted by shiny.tag
#' @param ... parameters passed to shiny.tag, if 'callbacks' list is among them, it is removed before it is passed to tagFactory
#' @return shiny.tag representing the produced parameter control
#'
.paramFactory <- function(name, label, r_class, group = 'default', 
                          tagFactory = function(...) {
                            do.call('tag', ...)
                          }, 
                          tagValueExtractor = .getValueFromTagAttribute, 
                          qsValueExtractor = .getSingleValueFromQueryParameter, 
                          nullValueProvider = function(value) {
                            if(is.null(value) || any(is.na(value))) {
                              ''
                            } else {
                              value
                            }
                          }, 
                          rToTagValueMapper = .rToUIControlValueMapper('input'), 
                          ...) {
  
  params_in <- list(...)
  
  ui.log.debug("Extra params: ", params_in)
  
  callbacks <- list()
  
  if ('callbacks' %in% names(params_in)) {
    ui.log.debug("Has custom callbacks.")
    callbacks <- .processCallbackFunctions(params_in[['callbacks']])
  }
  
  params_in$callbacks <- NULL
  
  
  ui.log.debug("Params for tag factory: ", params_in)
  inputTag <- do.call('tagFactory', params_in)
  
  defaultValue <- .getVariableValue(name)
  
  tagValue <- tagValueExtractor(inputTag)
  
  qsValue <- qsValueExtractor(name, r_class)
  
  value <- .selectValue(qsValue, tagValue, defaultValue)
  
  value <- nullValueProvider(value)
  
  if (length(value) > 0) {
    assign(name, value, envir=globalenv())
  }
  
  inputTag <- rToTagValueMapper(inputTag, value)
  
  control_descriptor <- .createControl(label, name, group, defaultValue, value, r_class, inputTag, callbacks)
  
  .registerControl(control_descriptor)
  
  return(control_descriptor$control_tag)  
}

.registerControl <- function(control_descriptor) {
  assign(control_descriptor$name, value = control_descriptor, envir = .params)
}

#'
#' Creates rcloud.params.control structure
#' 
#' @param label the label to be displayed in the UI
#' @param name the name of the variable
#' @param group the control group
#' @param default_value the default value of the variable (one specified by the associated variable)
#' @param param_value the value of the parameter resulting from runtime state: query string, variable value and shiny.tag parameter
#' @param r_class class of the variable
#' @param input_tag shiny.tag to be used for the input
#' @param user_callbacks the list of callbacks specified by the user
#' 
#' @return rcloud.params.control structure
#'
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
                 name = name,
                 callbacks = callbacks, 
                 r_class = r_class, 
                 control_tag = control_tag),
            class="rcloud.params.control")
  
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

.selectValue <- function(qsValue, tagValue, defaultValue) {
  if(!is.null(qsValue)) {
    qsValue
  } else if (!is.null(tagValue)) {
    tagValue    # If variable is undefined but user has set a value in widget, use this
  } else {
    defaultValue
  }
}

#' lookup variable value in globalenv
#'
.getVariableValue <- function(name) {
  vals <- mget(name, envir=globalenv(), ifnotfound=NA)
  val <- vals[[name]]
  if (any(is.na(val))) 
    val <- NULL
  val
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

.rToUIControlValueMapper <- function(control_type, choices = NULL) {
  localChoices <- choices
  switch(control_type, 
         'select' = function(tag, value) {
           options <- NULL
           if (is.null(names(localChoices))) {
             options <- list(lapply(localChoices, function(c) {
               res <- tags$option(c)
               typedChoice <- as.character(c)
               if (typedChoice %in% value)
                 res$attribs$selected = NA
               res
             }))
           } else {
             options <- list(lapply(names(localChoices), function(c) {
               res <- tags$option(localChoices[[c]], value=c)
               if (c %in% value)
                 res$attribs$selected = NA
               res
             })
             )
           }
           
           if (length(value) == 0) {
             if(!'multiple' %in% names(tag$attribs)) {
               # Add default disabled option to reflect lack of selection
               options <- c(list(tags$option('-- select --', 'hidden' = NA, 'disabled' = NA, 'selected' = NA, 'value' = NA)), options)
             }
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