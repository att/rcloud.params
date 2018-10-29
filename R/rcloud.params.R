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
  
  assign(name, value = control_descriptor, envir = .params)
  
  return(control_descriptor$control_tag)
}

#' 
#' Creates date-picker input control
#' 
#' @export
#' 
dateParam <- function(name, label = NULL, group = 'default', ...) {
  .paramFactory(name, label, 'Date', group, 
                tagFactory = function(...) {
                  inputTag <- tag('input', c(list(type = 'date'), list(...)))
                }, 
                ...)
  
}

#' 
#' Creates text input control
#' 
#' @export
#' 
textParam <- function(name, label = NULL, group = 'default', ...) {
  .paramFactory(name, label, 'character', group, 
                tagFactory = function(...) {
                  inputTag <- tag('input', c(list(type = 'text'), list(...)))
                }, 
                ...)
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
  .paramFactory(name, label, 'numeric', group, 
               tagFactory = function(...) {
                 params_in <- list(...)
                 type <- 'number'
                 if ('type' %in% names(params_in)) {
                   type <- params_in$type
                   params_in$type <- NULL
                 }
                 
                 inputTag <- tag('input', c(list(type = type), params_in))
               }, 
               min = min, max = max, ...)
}

#' 
#' Creates select input control
#' 
#' @export
#' 
selectParam <- function(name, label = NULL, choices = list(), group = 'default', ...) {
  .paramFactory(name, label, 'character', group, 
                tagFactory = function(...) {
                  tag('select', list(...))
                }, 
                tagValueExtractor = .getValueFromTagAttribute, 
                qsValueExtractor = .getMultiValueFromQueryParameter, 
                nullValueProvider = function(value) {
                  if(is.null(value) || any(is.na(value))) {
                    c()
                  } else {
                    value
                  }
                }, 
                rToTagValueMapper = .rToUIControlValueMapper('select', choices), 
                ...)
}

#' 
#' Creates checkbox input control
#' 
#' @export
#' 
checkboxParam <- function(name, label = NULL, group = 'default', ...) {
  
  .paramFactory(name, label, 'logical', group, 
                tagFactory = function(...) {
                  tags$input(type='checkbox', class="checkbox", ...)
                }, 
                tagValueExtractor = function(inputTag) {
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
                  tagValue
                }, 
                nullValueProvider = function(value) {
                  if(is.null(value) || is.na(value)) {
                    FALSE
                  } else {
                    value
                  }
                }, 
                rToTagValueMapper = .rToUIControlValueMapper('checkbox'), 
                ...)
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

.selectValue <- function(qsValue, tagValue, defaultValue) {
  if(!is.null(qsValue)) {
    qsValue
  } else if (!is.null(tagValue)) {
    tagValue    # If variable is undefined but user has set a value in widget, use this
  } else {
    defaultValue
  }
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