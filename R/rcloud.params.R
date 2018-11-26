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
  .logicalParam(name, ...)
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
logicalParam <- function(x, ...) {
  name <- deparse(substitute(x))
  .logicalParam(name, ...)
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


#' Add callback to existing parameter
#' 
#' @param type type of event - valid options are: `change`, `submit`, `click` (depending on the control type)
#' @param FUN callback function
#' 
#' @export
addCallback <- function(object, type, FUN) UseMethod("addCallback", object) 

#' @export
addCallback.default <- function(object, type, FUN) {
  name <- deparse(substitute(object))
  .addCallback(name, type, FUN)
}

#' Remove callbacks from existing parameter
#' 
#' @param type type of event - valid options are: `change`, `submit`, `click` (depending on the control type)
#' 
#' @export
removeCallbacks <- function(object, type) UseMethod("removeCallbacks", object) 

#' @export
removeCallbacks.default <- function(object, type) {
  name <- deparse(substitute(object))
  .removeCallbacks(name, type)
}

#' @export
print.rcloud.params.control <-
  function(x, ..., view = interactive()) {
    .ui.log.debug("Printing widget: ", x$id)
    rcloud.html.out(as.character(x$controlTag, rcloud_htmlwidgets_print = FALSE))
  }


#' Prints a param set form
#' 
#' Param set can have two modes:
#' * reactive - it doesn't perform busy wait, it halts notebook execution and passes control over execution to JS.
#' * synchronous (blocking) - it performs busy wait, and R keeps control over the execution process.
#' 
#' Whether to wait or not for a form is controlled by `waitIfInvalid` logical property of the `paramSet`. 
#' In case of reactive form, if the flag is set to `true` (default) and `on.submit` callback
#' is not defined, the form will be validated and an error produced if the form is invalid.
#'
#' @export
print.rcloud.params.param.set <- function(x, ..., view = interactive()) {
  
  if(x$hideSource) {
    rcloud.hide.source.current.cell()
  }
  
  # print shiny.tag
  print(x$content)
  
  .paramSetWaitCallback()(x)

  invisible(TRUE)
}

#' @export
as.character.rcloud.params.param.set <- function(x, ...) {
  as.character(x$content, ...)
}

#' @export
rcw.set.paramSet <- function(where, what) {
  rcw.set(where, as.character(what))
  .paramSetWaitCallback()(what)
}

#' @export
rcw.prepend.paramSet <- function(where, what) {
  rcw.prepend(where, as.character(what))
  .paramSetWaitCallback()(what)
}

#' @export
rcw.append.paramSet <- function(where, what) {
  rcw.append(where, as.character(what))
  .paramSetWaitCallback()(what)
}

.paramSetWaitCallback <- function() {
  function(x) {
    if (x$waitIfInvalid) {
      if (x$reactive) {
        # Only wait reactively if there is a submit event callback, otherwise synchronously validate the form and throw error if it is invalid
        if (!is.null(x$callbacks$submit) && is.list(x$callbacks$submit) && length(x$callbacks$submit) > 0 && is.function(x$callbacks$submit[[1]])) {
          waitForForm(x$name)
        } else {
          isValid <- validateForm(x$name)
          if(!isValid) {
            stop("Parameter values in form are invalid!")
          }
        }
      } else {
        controlValues <- waitForSynchronousForm(x$name)
        .ui.log.debug("Result", controlValues)
        if (!is.null(controlValues)) {
          
          lapply(controlValues, function(el) {
            if (exists(el$name, .params)) {
              control <- get(el$name, .params)
              typedValue <- control$uiToRValueMapper(el$value)
              assign(el$name, typedValue, envir=globalenv())
            }
          })
          
          if (!is.null(x$callbacks$submit) && is.list(x$callbacks$submit) && length(x$callbacks$submit) > 0 && is.function(x$callbacks$submit[[1]])) {
            do.call(x$callbacks$submit[[1]], list(x$name, controlValues))
          }
        }
      }
      
    }
  }
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
#' @param on.submit callback function to invoke when form is submitted
#' @param on.change callback function to invoke on each parameter of the param set change
#' @param name of the form
#' @param wait.if.invalid should notebook execution be stopped if parameter values are invalid, if `true` and `on.submit` function is not provided, an error will be produced if form is invalid
#' @param hide.source should the source of the cell displaying the form be hidden
#' 
#' @return rcloud.params.param.set structure 
#' 
#' @export 
paramSet <- function(..., 
                     name = paste0("form_", as.integer(runif(1)*1e6)), 
                     wait.if.invalid = TRUE, 
                     hide.source = FALSE) {
  
  paramsIn <- list(...)
  
  if (length(paramsIn) == 0) {
    stop('No parameters were provided!')
  }

  callbacks <- .processCallbackFunctions(paramsIn)

  paramsIn <- .removeCallbacksFromParams(paramsIn)

  paramsIn <- .registerCallbacksToChildElements(paramsIn, callbacks)

  content = tags$form(name = name, paramsIn)

  content$attribs[.rcloudHtmlwidgetsCompactAttr()] <- TRUE
  content$attribs[.rcloudParamsAttrNamespace()] <- TRUE
  
  if (wait.if.invalid && !is.null(callbacks$submit) && is.list(callbacks$submit) && 
      length(callbacks$submit) > 0 && is.function(callbacks$submit[[1]])) {
    content$attribs[.rcloudParamsAttr('wait')] <- TRUE
  }

  paramSetDescriptor <- structure(list(name = name, 
                                         content = content, 
                                         callbacks = callbacks, 
                                         reactive = TRUE, 
                                         waitIfInvalid = wait.if.invalid,
                                         hideSource = hide.source,
                                         uiToRValueMapper = function(x) { x }
                                         ), class="rcloud.params.param.set")
  
  .registerControl(paramSetDescriptor)
  
  return(paramSetDescriptor)
}

.registerCallbacksToChildElements <- function(params.in, callbacks) {
  lapply(params.in, function(par) {
    if ('shiny.tag' %in% class(par)) {
      parName <- par$attribs[[.rcloudParamsAttr('name')]]
      if (!is.null(parName)) {
        for (event in c('click','change')) {
          lapply(callbacks[event], function(c) {
            .addCallback(parName, event, c)
          })
        }
      }
    }
  })
  params.in
}


#' Creates synchronous param set form
#' 
#' Note! paramSet is not a shiny.tag, this means that it may not be wrapped in htmltools shiny.tag
#' 
#' @param ... child elements (shiny.tags)
#' @param on.submit callback function that should be invoked when form gets submitted
#' @param on.change callback function to invoke on each parameter of the param set change
#' @param name of the form
#' @param wait.if.invalid should notebook execution be blocked if parameter values are invalid
#' @param hide.source should the source of the cell displaying the form be hidden
#' 
#' @return rcloud.params.param.set structure 
#' 
#' @export 
synchronousParamSet <- function(...,
                                name = paste0("form_", as.integer(runif(1)*1e6)), 
                                wait.if.invalid = TRUE, 
                                hide.source = FALSE) {
  
  paramsIn <- list(...)
  
  if (length(paramsIn) == 0) {
    stop('No parameters were provided!')
  }
  
  callbacks <- .processCallbackFunctions(paramsIn)
  
  paramsIn <- .removeCallbacksFromParams(paramsIn)
  
  paramsIn <- .registerCallbacksToChildElements(paramsIn, callbacks)
  
  content = tags$form(name = name, paramsIn)
  content$attribs[.rcloudHtmlwidgetsCompactAttr()] <- TRUE
  content$attribs[.rcloudParamsAttrNamespace()] <- TRUE
  
  if (wait.if.invalid) {
    content$attribs[.rcloudParamsAttr('wait')] <- TRUE
  }
  
  paramSetDescriptor <- structure(list(name = name, 
                                         content = content, 
                                         callbacks = callbacks, 
                                         reactive = FALSE, 
                                         waitIfInvalid = wait.if.invalid,
                                         hideSource = hide.source
                                         ), class="rcloud.params.param.set")
  
  .registerControl(paramSetDescriptor)
  
  return(paramSetDescriptor)
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
    paramsIn <- list(...)
    
    .ui.log.debug("Extra params: ", paramsIn)
    
    inputTag <-
      tags$button(value,
                  id = name,
                  type = 'submit',
                  paramsIn,
                  class = "btn btn-primary")
    
    controlDescriptor <-
      .createControl(label,
                     name,
                     .uiToRValueMapper('logical'),
                     inputTag
      )
    .registerControl(controlDescriptor)
    
    return(controlDescriptor$controlTag)
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
    paramsIn <- list(...)
    
    .ui.log.debug("Extra params: ", paramsIn)
    
    callbacks <- .processCallbackFunctions(paramsIn)
    
    paramsIn <- .removeCallbacksFromParams(paramsIn)
    
    inputTag <-
      tags$button(value,
                  id = name,
                  type = 'button',
                  class = "btn btn-default", 
                  paramsIn)
    
    controlDescriptor <-
      .createControl(label,
                     name,
                     .uiToRValueMapper('logical'),
                     inputTag,
                     callbacks)
    .registerControl(controlDescriptor)
    return(controlDescriptor$controlTag)
  }

#'
#' Creates date-picker input control
#'
.dateParam <- function(name,
                      label = NULL,
                      r.class = 'Date',
                      tag.factory = function(...) {
                        inputTag <- tag('input', c(list(type = 'date'), list(...)))
                      },
                      ...) {
  
  .paramFactory(
    name,
    label,
    r.class = r.class,
    tag.factory = tag.factory,
    ...
  )
  
}

#'
#' Creates text input control
#' 
.textParam <- function(name,
                      label = NULL,
                      r.class = 'character',
                      tag.factory = function(...) {
                        inputTag <- tag('input', c(list(type = 'text'), list(...)))
                      },
                      ...) {
  .paramFactory(
    name,
    label,
    r.class = r.class,
    tag.factory = tag.factory,
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
           empty.value.handler = .erroringEmptyValueHandler,
           ...) {
    .numericParam(name,
                 label,
                 min,
                 max,
                 type = 'range',
                 class = 'form-control slider',
                 empty.value.handler = empty.value.handler,
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
           r.class = 'numeric',
           tag.factory = function(...) {
             paramsIn <- list(...)
             type <- 'number'
             if ('type' %in% names(paramsIn)) {
               type <- paramsIn$type
               paramsIn$type <- NULL
             }
             
             inputTag <-
               tag('input', c(list(type = type), paramsIn))
           },
           ...) {
    .paramFactory(
      name,
      label,
      min = min,
      max = max,
      r.class = r.class,
      tag.factory = tag.factory,
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
           r.class = 'character',
           tag.factory = function(...) {
             tag('select', list(...))
           },
           tag.value.extractor = .getValueFromTagAttribute,
           qs.value.extractor = .getMultiValueFromQueryParameter,
           empty.value.handler = function(var, val) {
             c()
           },
           r.to.tag.value.mapper = .rToUIControlSelectValueMapper(choices),
           ...) {
    .paramFactory(
      name,
      label,
      r.class,
      tag.factory = tag.factory,
      tag.value.extractor = tag.value.extractor,
      qs.value.extractor = qs.value.extractor,
      empty.value.handler = empty.value.handler,
      r.to.tag.value.mapper = r.to.tag.value.mapper,
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
           r.class = 'character',
           tag.factory = function(...) {
             res <- tag('div', list(...))
             res$attribs[.rcloudParamsAttr('radio-group-name')] = name
             res
           },
           r.to.tag.value.mapper = .rToUIControlRadioValueMapper(choices),
           ...) {
    .paramFactory(
      name,
      label,
      r.class = r.class,
      tag.factory = tag.factory,
      r.to.tag.value.mapper = r.to.tag.value.mapper,
      ...
    )
  }

#'
#' Creates checkbox input control
#' 
#'
.logicalParam <-
  function(name,
           label = NULL,
           r.class = 'logical',
           tag.factory = function(...) {
             tags$input(type = 'checkbox', class = "checkbox", ...)
           },
           empty.value.handler = .erroringEmptyValueHandler,
           tag.value.extractor = function(inputTag) {
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
           r.to.tag.value.mapper = .rToUIControlCheckboxValueMapper(),
           ...) {
    .paramFactory(
      name,
      label,
      r.class = r.class,
      tag.factory = tag.factory,
      empty.value.handler = empty.value.handler,
      tag.value.extractor = tag.value.extractor,
      r.to.tag.value.mapper = r.to.tag.value.mapper,
      ...
    )
  }
