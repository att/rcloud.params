#' Creates parameter for a variable
#' 
#' A simple way of exposing a variable as notebook parameter. Creates a parameter based on the variable type being passed in.
#' 
#' Just a basic set of controls are supported, to create radio buttons, numeric sliders and select inputs please use specific `*Param` functions.
#' 
#' @param object variable which should be exposed as a parameter
#' @param ... extra parameters to specific implementations of `*Param` functions
#' 
#' @return parameter shiny.tag
#' 
#' @export
param <- function(object,...) UseMethod("param", object) 

#' @export
param.Date <- function(x, ...) {
  name <- deparse(substitute(x))
  .dateParam(name, ...)
}

#' @export
param.logical <- function(x, ...) {
  name <- deparse(substitute(x))
  .checkboxParam(name, ...)
}

#' @export
param.character <- function(x, ...) {
  name <- deparse(substitute(x))
  .textParam(name, ...)
}

#' @export
param.numeric <- function(x, ...) {
  name <- deparse(substitute(x))
  .numericParam(name, ...)
}

#' @export
dateParam <- function(x, ...) {
  name <- deparse(substitute(x))
  .dateParam(name, ...)
}

#' @export
numericParam <-  function(x, ...) {
  name <- deparse(substitute(x))
  .numericParam(name, ...)
}

#' @export
numericSliderParam <- function(x, ...) {
  name <- deparse(substitute(x))
  .numericSliderParam(name, ...)
}

#' @export
checkboxParam <- function(x, ...) {
  name <- deparse(substitute(x))
  .checkboxParam(name, ...)
}

#' @export
textParam <- function(x, ...) {
  name <- deparse(substitute(x))
  .textParam(name, ...)
}

#' @export
selectParam <- function(x, ...) {
  name <- deparse(substitute(x))
  .selectParam(name, ...)
}

#' @export
choiceParam <-  function(x, ...) {
  name <- deparse(substitute(x))
  .choiceParam(name, ...)
}

#' @export
print.rcloud.params.control <-
  function(x, ..., view = interactive()) {
    ui.log.debug("Printing widget: ", x$id)
    rcloud.html.out(as.character(x$control_tag, rcloud_htmlwidgets_print = FALSE))
  }


#' Prints a param set form
#'
#' @export
print.rcloud.params.param.set <- function(x, ..., view = interactive()) {
  print(x$content)
  
  if(x$wait_for) {
    controlValues <- waitForForm(x$name)
    
    if(!is.null(controlValues)) {
      lapply(controlValues, function(el) {
        control <- get(el$name, .params)
        typed_value <- control$uiToRValueMapper(el$value)
        assign(el$name, typed_value, envir=globalenv())
      });
    }
  }
  invisible(TRUE)
}

#' Display shiny tag without extra iframe
#' 
#' @return given shiny tag decorated with rcloud-htmlwidgets attribute
#' 
#' @export

compact <- function(tag) {
  tag$attribs[.rcloudHtmlwidgetsCompactAttr()] <- TRUE
  tag
}

#' Produces a div with additional attribute ('data-rcloud-htmlwidgets-compact') 
#' 
#' The resulting div will not be wrapped by iframe by rcloud.htmlwidgets.
#' 
#' @param ... htmltools div function parameters
#' @return shiny.tag 
#'  
#' @export
paramDiv <- function(...) {
  divTag <- div(...)
  divTag <- compact(divTag)
  return(divTag)
}

#' Creates param set form
#' 
#' Note! paramSet is not a shiny.tag, this means that it may not be wrapped in htmltools shiny.tag
#' 
#' @param ... child elements (shiny.tags)
#' @param callbacks form callbacks
#' @param wait_for should the R process be blocked when form is displayed
#' @param name of the form
#' 
#' @return rcloud.params.param.set structure 
#' 
#' @export 
paramSet <- function(..., callbacks = list(), wait_for = FALSE, name = paste0("form_", as.integer(runif(1)*1e6))) {
  
  in_params <- list(...)
  
  if (length(in_params) == 0) {
    stop('No parameters were provided!')
  }
  
  content = tags$form(name = name, in_params)
  content$attribs[.rcloudHtmlwidgetsCompactAttr()] <- TRUE
  content$attribs[.rcloudParamsAttrNamespace()] <- TRUE
  
  param_set_descriptor <- structure(list(name = name, content = content, callbacks = callbacks, wait_for = wait_for), class="rcloud.params.param.set")
  
  .registerControl(param_set_descriptor)
  
  return(param_set_descriptor)
}

#'
#' Create submit button
#' 
#' @export
submitParam <-
  function(name = paste0("submit_", as.integer(runif(1) * 1e6)),
           value = 'Submit',
           label = '',
           ...) {
    params_in <- list(...)
    
    ui.log.debug("Extra params: ", params_in)
    
    callbacks <- list()
    
    if ('callbacks' %in% names(params_in)) {
      callbacks <- .processCallbackFunctions(params_in[['callbacks']])
    }
    
    params_in['callbacks'] <- NULL
    
    inputTag <-
      tags$button(value,
                  id = name,
                  type = 'submit',
                  params_in,
                  class = "btn btn-default")
    
    control_descriptor <-
      .createControl(label,
                     name,
                     .uiToRValueMapper('logical'),
                     inputTag,
                     callbacks)
    
    return(control_descriptor$control_tag)
  }

#'
#' Create action button
#' 
#' @export
buttonParam <-
  function(name = paste0("button_", as.integer(runif(1) * 1e6)),
           value = 'Button',
           label = '',
           ...) {
    params_in <- list(...)
    
    ui.log.debug("Extra params: ", params_in)
    
    callbacks <- list()
    
    if ('callbacks' %in% names(params_in)) {
      callbacks <- .processCallbackFunctions(params_in[['callbacks']])
    }
    
    params_in['callbacks'] <- NULL
    
    inputTag <-
      tags$button(value,
                  id = name,
                  type = 'button',
                  class = "btn btn-default", 
                  params_in)
    
    control_descriptor <-
      .createControl(label,
                     name,
                     .uiToRValueMapper('logical'),
                     inputTag,
                     callbacks)
    .registerControl(control_descriptor)
    return(control_descriptor$control_tag)
  }

#'
#' Creates date-picker input control
#'
.dateParam <- function(name,
                      label = NULL,
                      ...) {
  
  .paramFactory(
    name,
    label,
    'Date',
    tagFactory = function(...) {
      inputTag <- tag('input', c(list(type = 'date'), list(...)))
    },
    ...
  )
  
}

#'
#' Creates text input control
#' 
.textParam <- function(name,
                      label = NULL,
                      ...) {
  .paramFactory(
    name,
    label,
    'character',
    tagFactory = function(...) {
      inputTag <- tag('input', c(list(type = 'text'), list(...)))
    },
    ...
  )
}

#'
#' Creates numeric slider input control
#'
.numericSliderParam <-
  function(name,
           label = NULL,
           min = NA,
           max = NA,
           ...) {
    .numericParam(name,
                 label,
                 min,
                 max,
                 type = 'range',
                 class = 'form-control slider',
                 ...)
  }
#'
#' Creates numeric input control
#'
.numericParam <-
  function(name,
           label = NULL,
           min = NA,
           max = NA,
           ...) {
    .paramFactory(
      name,
      label,
      'numeric',
      tagFactory = function(...) {
        params_in <- list(...)
        type <- 'number'
        if ('type' %in% names(params_in)) {
          type <- params_in$type
          params_in$type <- NULL
        }
        
        inputTag <-
          tag('input', c(list(type = type), params_in))
      },
      min = min,
      max = max,
      ...
    )
  }

#'
#' Creates select input control
#'
.selectParam <-
  function(name,
           label = NULL,
           choices = list(),
           ...) {
    .paramFactory(
      name,
      label,
      'character',
      tagFactory = function(...) {
        tag('select', list(...))
      },
      tagValueExtractor = .getValueFromTagAttribute,
      qsValueExtractor = .getMultiValueFromQueryParameter,
      nullValueProvider = function(value) {
        if (is.null(value) || any(is.na(value))) {
          c()
        } else {
          value
        }
      },
      rToTagValueMapper = .rToUIControlSelectValueMapper(choices),
      ...
    )
  }

#'
#' Creates radio buttons group input control
#'
.choiceParam <-
  function(name,
           label = NULL,
           choices = list(),
           ...) {
    .paramFactory(
      name,
      label,
      'character',
      tagFactory = function(...) {
        res <- tag('div', list(...))
        res$attribs[.rcloudParamsAttr('radio-group-name')] = name
        res
      },
      rToTagValueMapper = .rToUIControlRadioValueMapper(choices),
      ...
    )
  }

#'
#' Creates checkbox input control
#'
.checkboxParam <-
  function(name,
           label = NULL,
           ...) {
    .paramFactory(
      name,
      label,
      'logical',
      tagFactory = function(...) {
        tags$input(type = 'checkbox', class = "checkbox", ...)
      },
      tagValueExtractor = function(inputTag) {
        tagValue <- NULL
        if ('checked' %in% names(inputTag$attribs)) {
          if (!is.null(inputTag$attribs$checked) &&
              !any(is.na(inputTag$attribs$checked))) {
            tagValue <- if (is.logical(inputTag$attribs$checked)) {
              inputTag$attribs$checked
            } else {
              TRUE
            }
          }
        }
        tagValue
      },
      nullValueProvider = function(value) {
        if (is.null(value) || is.na(value)) {
          FALSE
        } else {
          value
        }
      },
      rToTagValueMapper = .rToUIControlCheckboxValueMapper(),
      ...
    )
  }
