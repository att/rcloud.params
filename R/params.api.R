#'
#' Factory creating rcloud.params controls
#' 
#' 
#' The factory is configurable via a number of strategies that address variability in the process of creating shiny.tags representing rcloud.params controls.
#' 
#' The returned value is a shiny.tag structure, the associated rcloud.params.control is persisted in the packages environment and is used to invoke control-specific callback functions.
#' 
#' @param var the variable name
#' @param label the label for the control
#' @param r_class type of the variable
#' @param query variable parameter name
#' @param tagFactory the function returning form input shiny.tags
#' @param tagValueExtractor a function that extracts value specified via shiny.tag attributes
#' @param qsValueExtractor a function that extracts value from a query string
#' @param nullValueProvider a function that returns a value that should be used if default, query string nor shiny.tag specify a value
#' @param rToTagValueMapper a mapper that converts R value to a value accepted by shiny.tag
#' @param uiToRValueMapper a mapper that converts UI value to an R value
#' @param ... parameters passed to shiny.tag, if 'callbacks' list is among them, it is removed before it is passed to tagFactory
#' @return shiny.tag representing the produced parameter control
#'
.paramFactory <- function(var, label, r_class, query = var,
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
                          rToTagValueMapper = .rToUIControlDefaultValueMapper(.rToUIValueMapper(r_class)),
                          uiToRValueMapper = .uiToRValueMapper(r_class),
                          ...) {
  
  params_in <- list(...)
  
  ui.log.debug("Extra params: ", params_in)
  
  callbacks <- list()
  
  if ('callbacks' %in% names(params_in)) {
    callbacks <- .processCallbackFunctions(params_in$callbacks)
  }
  params_in$callbacks <- NULL
  
  if ('on_change' %in% names(params_in)) {
    if (!'change' %in% names(callbacks)) {
      callbacks$change <- list()
    }
    callbacks$change <- c(callbacks$change, params_in$on_change)
    params_in$on_change <- NULL
  }
  
  
  if (!'required' %in% names(params_in)) {
    params_in$required <- NA
  } else {
    if (is.logical(params_in$required) && !params_in$required) {
      params_in$required <- NULL
    }
  }
  
  
  ui.log.debug("Params for tag factory: ", params_in)
  inputTag <- do.call('tagFactory', params_in)
  
  defaultValue <- .getVariableValue(var)
  
  tagValue <- tagValueExtractor(inputTag)
  
  qsValue <- qsValueExtractor(query, uiToRValueMapper)
  
  value <- .selectValue(qsValue, tagValue, defaultValue)
  
  value <- nullValueProvider(value)
  
  if (length(value) > 0) {
    assign(var, value, envir=globalenv())
  }
  
  ui.log.debug("Parameter - ", paste0("Var: ", var, ", Par name: ", query,
                                      ", Default: ", paste(defaultValue, collapse = ","), 
                                      ", Default type: ", typeof(defaultValue), 
                                      ", Current: ", paste(value, collapse=","), 
                                      ", Current type: ", typeof(value), 
                                      ", Class: ", r_class))
  
  inputTag <- rToTagValueMapper(inputTag, defaultValue, value)
  
  control_descriptor <- .createControl(label, var, uiToRValueMapper, inputTag, callbacks, par_name = query)
  
  .registerControl(control_descriptor)
  
  return(control_descriptor$control_tag)  
}

.registerControl <- function(control_descriptor) {
  assign(control_descriptor$name, value = control_descriptor, envir = .params)
}

.addCallback <- function(param, type, FUN = NULL) {
  if(!is.null(FUN)) {
    if(!exists(param, envir = .params)) {
      stop(paste0("Parameter '" +  param + "' does not exist."))
    }
    control <- get(param, envir = .params)
    control$callbacks[[type]] <- c(control$callbacks[[type]], FUN)
    .registerControl(control)
  }
}

#'
#' Creates rcloud.params.control structure
#' 
#' @param label the label to be displayed in the UI
#' @param var_name the name of the variable
#' @param uiToRValueMapper function mapping UI values to R
#' @param input_tag shiny.tag to be used for the input
#' @param user_callbacks the list of callbacks specified by the user
#' @param par_name parameter name, as used by the UI
#' 
#' @return rcloud.params.control structure
#'
.createControl <- function(label, var_name, uiToRValueMapper, input_tag, user_callbacks = list(), par_name = var_name) {
  label_id <- .controlLabelId(par_name)
  input_id <- .controlInputId(par_name)
  control_id <- .controlId(par_name)
  input_tag$attribs$id <- input_id
  
  parNameAttr <- .rcloudParamsAttr('name')
  
  input_tag$attribs[parNameAttr] <- par_name
  input_tag$attribs[.rcloudHtmlwidgetsCompactAttr()] <- TRUE
  
  labelMsg <- label
  
  if('type' %in% names(input_tag$attribs) && input_tag$attribs$type == 'checkbox') {
    # just checkbox is a special case...
    input_tag$attribs$class <- NULL
    
    label_tag <- tags$label(labelMsg,
                            id = label_id,
                            'for' = input_id)
    
    control_tag <- paramDiv(id = control_id,
                            'class' = 'form-group', div(class='checkbox', label_tag, input_tag));
  } else {
    if (input_tag$name != 'div') {
      if (is.null(input_tag$attribs$class)) {
        input_tag$attribs$class <- 'form-control'
      }
    }
    label_tag <- tags$label(labelMsg, 
                            id = label_id,
                            'class' = 'control-label',
                            'for' = input_id)
    
    control_tag <- paramDiv(id = control_id, 
                            'class' = 'form-group', label_tag, input_tag)
  }
  
  control_tag$attribs[.rcloudParamsAttrNamespace()] = TRUE
  control_tag$attribs[parNameAttr] = par_name
  
  assignValueCallback <- function(var_name, var_value, ...) {
    ui.log.debug(var_name, paste0(var_value, collapse = ","), typeof(var_value))
    assign(var_name, var_value, envir=globalenv());
  } # make call back ocap so variable created in js side can be assigned back to R
  
  callbacks <- .prependCallbackFunction(user_callbacks, 'change', assignValueCallback)
  
  structure(list(id = control_id,
                 name = par_name,
                 var_name = var_name,
                 callbacks = callbacks, 
                 uiToRValueMapper = uiToRValueMapper, 
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

.rcloudHtmlwidgetsCompactAttr <- function() {
  'data-rcloud-htmlwidgets-compact'
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

.getQueryParams <- function(name) {
  qparams <- get(QUERY_PARAMS, envir = .query.params)
  qsValue <- qparams[[name]]
  ui.log.debug('QS value:', qsValue)
  qsValue
}

.getMultiValueFromQueryParameter <- function(name, valueMapper) {
  qsValue <- .getQueryParams(name)
  value <- NULL
  if (!is.null(qsValue[1])) {
    parser <- .qsMultiValueParser()
    value <- valueMapper(parser(qsValue))
  }
  value
}

.getSingleValueFromQueryParameter <- function(name, valueMapper) {
  qsValue <- .getQueryParams(name)
  value <- NULL
  if (!is.null(qsValue[1])) {
    parser <- .qsSingleValueParser()
    value <- valueMapper(parser(qsValue))
  }
  value
}

.qsMultiValueParser <- function() {
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
  
  isNotEmpty <- function(val) {
    !is.null(val) && !any(is.na(val)) && val != ''
  }
  asCharacterMapper <- function(val) {
    if (isNotEmpty(val)) {
      tryCatch(as.character(val), error = function(e) {
        NULL
      })
    } else {
      NULL
    }
  }
  
  switch(r_class, 
         'character' = asCharacterMapper,
         'Date' = function(val) {
           if(isNotEmpty(val)) {
             tryCatch(as.Date(val), error = function(e) {
               NULL
             })
           } else {
             NULL
           }
         },
         'numeric' = function(val) {
           if(isNotEmpty(val)) {
             tryCatch(as.numeric(val), error = function(e) {
               NULL
             })
           } else {
             NULL
           }
         }, 
         'logical' = function(val) {
           if(isNotEmpty(val)) {
             tryCatch(as.logical(val), error = function(e) {
               NULL
             })
           } else {
             NULL
           }
         }, 
         asCharacterMapper)
}

.rToUIValueMapper <- function(r_class) {
  
  switch(r_class, 
         'character' = function(x) { x },
         'Date' = .toCharacterSafe,
         'numeric' = .toCharacterSafe, 
         'logical' = .toCharacterSafe, 
         .toCharacterSafe)
}

.toCharacterSafe <- function(val) {
  if(!is.null(val) && !is.character(val)) {
    as.character(val)
  } else if(is.character(val)) {
    val
  } else {
    ''
  }
  
}

.rToUIControlDefaultValueMapper <- function(rToUIValueMapper = .toCharacterSafe) {
  valueMapper = rToUIValueMapper
  function(tag, defaultValue, value) {
    if (!is.null(value)) {
      tag$attribs$value <- valueMapper(value)
    }
    if (!is.null(defaultValue)) {
      tag$attribs[.rcloudParamsAttr('default-value')] <- valueMapper(defaultValue)
    }
    tag
  }
}

.rToUIControlSelectValueMapper <- function(choices = NULL, rToUIValueMapper = .toCharacterSafe) {
  valueMapper = rToUIValueMapper
  localChoices <- choices
  function(tag, defaultValue, value) {
    options <- NULL
    if (is.null(names(localChoices))) {
      options <- list(lapply(localChoices, function(c) {
        typedChoice <- valueMapper(c)
        res <- tags$option(typedChoice)
        if (typedChoice %in% valueMapper(value))
          res$attribs$selected <- NA
        if(typedChoice %in% valueMapper(defaultValue)) {
          res$attribs[.rcloudParamsAttr('default-value')] <- 'true';
        }
        res
      }))
    } else {
      options <- list(lapply(names(localChoices), function(c) {
        res <- tags$option(localChoices[[c]], value=c)
        if (c %in% valueMapper(value))
          res$attribs$selected <- NA
        if(c %in% valueMapper(defaultValue)) {
          res$attribs[.rcloudParamsAttr('default-value')] <- 'true';
        }
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
  }
}

.rToUIControlCheckboxValueMapper <- function() {
  function(tag, defaultValue, value) {
           tag$attribs$value <- NULL
           if (value) {
             tag$attribs$checked <- value
           } else {
             tag$attribs$checked <- NULL
           }
           
           if(is.null(defaultValue) || !defaultValue) {
             tag$attribs[.rcloudParamsAttr('default-value')] <- 'false';
           } else {
             tag$attribs[.rcloudParamsAttr('default-value')] <- 'true';
           }
           tag
         }
}

.rToUIControlRadioValueMapper <- function(choices = NULL, rToUIValueMapper = .toCharacterSafe) {
  valueMapper <- rToUIValueMapper
  localChoices <- choices
  function(tag, defaultValue, value) {
    options <- NULL
    required <- if('required' %in% names(tag$attribs)) {
      TRUE
    } else {
      FALSE
    }
    if (is.null(names(localChoices))) {
      options <- list(lapply(localChoices, function(c) {
        typedChoice <- valueMapper(c)
        res <- tags$input(type='radio', name = tag$attribs[.rcloudParamsAttr('radio-group-name')], value = typedChoice)
        if (typedChoice %in% valueMapper(value))
          res$attribs$checked <- NA
        if(typedChoice %in% valueMapper(defaultValue)) {
          res$attribs[.rcloudParamsAttr('default-value')] <- 'true';
        }
        if(required) {
          res$attribs$required <- NA
        }
        div(class="radio", tags$label(typedChoice, res))
      }))
    } else {
      options <- list(lapply(names(localChoices), function(c) {
        res <- tags$input(type='radio', name = tag$attribs[.rcloudParamsAttr('radio-group-name')], value=c)
        if (c %in% valueMapper(value))
          res$attribs$checked <- NA
        if(c %in% valueMapper(defaultValue)) {
          res$attribs[.rcloudParamsAttr('default-value')] <- 'true';
        }
        if(required) {
          res$attribs$required <- NA
        }
        div(class="radio", tags$label(localChoices[[c]], res))
      })
      )
    }
    
    tag$children <- options
    tag$attribs$choices <- NULL
    tag$attribs$value <- NULL
    tag$attribs$required <- NULL
    tag
  }
}

.getValueFromTagAttribute <- function(inputTag) {
  tagValue <- NULL
  if ('value' %in% names(inputTag$attribs)) {
    if (!is.null(inputTag$attribs$value) && !any(is.na(inputTag$attribs$value))) {
      tagValue <- inputTag$attribs$value
    }
  }
  tagValue
}

.processCallbackFunctions <- function(callbacksParam = list()) {
  callbacks <- list()
  for (callback in names(callbacksParam)) {
    if (!is.list(callbacksParam[[callback]])) {
      callbacks[callback] <- list(callbacksParam[callback])
    } else {
      callbacks[callback] <- callbacksParam[callback]
    }
  }
  invisible(callbacks)
}

.prependCallbackFunction <- function(callbacks = list(), eventType = 'change', FUN) {
  if (is.null(eventType) || is.null(FUN)) {
    callbacks
  } else {
    if (eventType %in% names(callbacks)) {
      callbacks[[eventType]] <- append(list(FUN), callbacks[[eventType]])
    } else {
      callbacks[[eventType]] <- list(FUN)
    }
    callbacks
  }
}