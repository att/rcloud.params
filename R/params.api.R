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
#' @param r.class type of the variable
#' @param query variable parameter name
#' @param tag.factory the function returning form input shiny.tags
#' @param tag.value.extractor a function that extracts value specified via shiny.tag attributes
#' @param qs.value.extractor a function that extracts value from a query string
#' @param null.value.provider a function that returns a value that should be used if default, query string nor shiny.tag specify a value
#' @param r.to.tag.value.mapper a mapper that converts R value to a value accepted by shiny.tag
#' @param ui.to.r.value.mapper a mapper that converts UI value to an R value
#' @param ... parameters passed to shiny.tag, if 'callbacks' list is among them, it is removed before it is passed to tag.factory
#' @return shiny.tag representing the produced parameter control
#'
.paramFactory <- function(var, label, r.class, query = var,
                          tag.factory = function(...) {
                            do.call('tag', ...)
                          }, 
                          tag.value.extractor = .getValueFromTagAttribute, 
                          qs.value.extractor = .getSingleValueFromQueryParameter, 
                          null.value.provider = function(value) {
                            if(is.null(value) || any(is.na(value))) {
                              ''
                            } else {
                              value
                            }
                          }, 
                          r.to.tag.value.mapper = .rToUIControlDefaultValueMapper(.rToUIValueMapper(r.class)),
                          ui.to.r.value.mapper = .uiToRValueMapper(r.class),
                          ...) {
  
  paramsIn <- list(...)
  
  ui.log.debug("Extra params: ", paramsIn)
  
  callbacks <- list()
  
  if ('callbacks' %in% names(paramsIn)) {
    callbacks <- .processCallbackFunctions(paramsIn$callbacks)
  }
  paramsIn$callbacks <- NULL
  
  if ('on.change' %in% names(paramsIn)) {
    if (!'change' %in% names(callbacks)) {
      callbacks$change <- list()
    }
    callbacks$change <- c(callbacks$change, paramsIn$on.change)
    paramsIn$on.change <- NULL
  }
  
  
  if (!'required' %in% names(paramsIn)) {
    paramsIn$required <- NA
  } else {
    if (is.logical(paramsIn$required) && !paramsIn$required) {
      paramsIn$required <- NULL
    }
  }
  
  
  ui.log.debug("Params for tag factory: ", paramsIn)
  inputTag <- do.call('tag.factory', paramsIn)
  
  defaultValue <- .getVariableValue(var)
  
  tagValue <- tag.value.extractor(inputTag)
  
  qsValue <- qs.value.extractor(query, ui.to.r.value.mapper)
  
  value <- .selectValue(qsValue, tagValue, defaultValue)
  
  value <- null.value.provider(value)
  
  if (length(value) > 0) {
    assign(var, value, envir=globalenv())
  }
  
  ui.log.debug("Parameter - ", paste0("Var: ", var, ", Par name: ", query,
                                      ", Default: ", paste(defaultValue, collapse = ","), 
                                      ", Default type: ", typeof(defaultValue), 
                                      ", Current: ", paste(value, collapse=","), 
                                      ", Current type: ", typeof(value), 
                                      ", Class: ", r.class))
  
  inputTag <- r.to.tag.value.mapper(inputTag, defaultValue, value)
  
  controlDescriptor <- .createControl(label, var, ui.to.r.value.mapper, inputTag, callbacks, par.name = query)
  
  .registerControl(controlDescriptor)
  
  return(controlDescriptor$controlTag)  
}

.registerControl <- function(control.descriptor) {
  assign(control.descriptor$name, value = control.descriptor, envir = .params)
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
#' @param var.name the name of the variable
#' @param ui.to.r.value.mapper function mapping UI values to R
#' @param input.tag shiny.tag to be used for the input
#' @param user.callbacks the list of callbacks specified by the user
#' @param par.name parameter name, as used by the UI
#' 
#' @return rcloud.params.control structure
#'
.createControl <- function(label, var.name, ui.to.r.value.mapper, input.tag, user.callbacks = list(), par.name = var.name) {
  labelId <- .controlLabelId(par.name)
  inputId <- .controlInputId(par.name)
  controlId <- .controlId(par.name)
  input.tag$attribs$id <- inputId
  
  parNameAttr <- .rcloudParamsAttr('name')
  
  input.tag$attribs[parNameAttr] <- par.name
  input.tag$attribs[.rcloudHtmlwidgetsCompactAttr()] <- TRUE
  
  labelMsg <- label
  
  if('type' %in% names(input.tag$attribs) && input.tag$attribs$type == 'checkbox') {
    # just checkbox is a special case...
    input.tag$attribs$class <- NULL
    
    labelTag <- tags$label(labelMsg,
                            id = labelId,
                            'for' = inputId)
    
    controlTag <- paramDiv(id = controlId,
                            'class' = 'form-group', div(class='checkbox', labelTag, input.tag));
  } else {
    if (input.tag$name != 'div') {
      if (is.null(input.tag$attribs$class)) {
        input.tag$attribs$class <- 'form-control'
      }
    }
    labelTag <- tags$label(labelMsg, 
                            id = labelId,
                            'class' = 'control-label',
                            'for' = inputId)
    
    controlTag <- paramDiv(id = controlId, 
                            'class' = 'form-group', labelTag, input.tag)
  }
  
  controlTag$attribs[.rcloudParamsAttrNamespace()] = TRUE
  controlTag$attribs[parNameAttr] = par.name
  
  assignValueCallback <- function(var.name, var.value, ...) {
    ui.log.debug(var.name, paste0(var.value, collapse = ","), typeof(var.value))
    assign(var.name, var.value, envir=globalenv());
  } # make call back ocap so variable created in js side can be assigned back to R
  
  callbacks <- .prependCallbackFunction(user.callbacks, 'change', assignValueCallback)
  
  structure(list(id = controlId,
                 name = par.name,
                 varName = var.name,
                 callbacks = callbacks, 
                 uiToRValueMapper = ui.to.r.value.mapper, 
                 controlTag = controlTag),
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

.uiToRValueMapper <- function(r.class) {
  
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
  
  switch(r.class, 
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

.rToUIValueMapper <- function(r.class) {
  
  switch(r.class, 
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

.rToUIControlDefaultValueMapper <- function(r.to.ui.value.mapper = .toCharacterSafe) {
  valueMapper = r.to.ui.value.mapper
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

.rToUIControlSelectValueMapper <- function(choices = NULL, r.to.ui.value.mapper = .toCharacterSafe) {
  valueMapper = r.to.ui.value.mapper
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

.rToUIControlRadioValueMapper <- function(choices = NULL, r.to.ui.value.mapper = .toCharacterSafe) {
  valueMapper <- r.to.ui.value.mapper
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

.processCallbackFunctions <- function(callbacks.param = list()) {
  callbacks <- list()
  for (callback in names(callbacks.param)) {
    if (!is.list(callbacks.param[[callback]])) {
      callbacks[callback] <- list(callbacks.param[callback])
    } else {
      callbacks[callback] <- callbacks.param[callback]
    }
  }
  invisible(callbacks)
}

.prependCallbackFunction <- function(callbacks = list(), event.type = 'change', FUN) {
  if (is.null(event.type) || is.null(FUN)) {
    callbacks
  } else {
    if (event.type %in% names(callbacks)) {
      callbacks[[event.type]] <- append(list(FUN), callbacks[[event.type]])
    } else {
      callbacks[[event.type]] <- list(FUN)
    }
    callbacks
  }
}