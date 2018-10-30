
#'
#'  Blocks execution and waits for submission of a form associated with the given group
#' 
#' @export
waitForGroup <- function(group = 'default') {
  input.caps$wait_for_group(Rserve.context(), group)
}

#' @export
print.rcloud.params.control <-
  function(x, ..., view = interactive()) {
    ui.log.debug("Printing widget: ", x$id)
    rcloud.html.out(as.character(x$control_tag, rcloud_htmlwidgets_print = FALSE))
  }


#'
#' Create submit button
#' 
#' @export
submitParam <-
  function(name = paste0("submit_", as.integer(runif(1) * 1e6)),
           value = 'Submit',
           label = '',
           group = 'default',
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
                     group,
                     FALSE,
                     FALSE,
                     'logical',
                     inputTag,
                     callbacks)
    
    return(control_descriptor$control_tag)
  }

#'
#' Creates date-picker input control
#'
#' @export
#'
dateParam <- function(name,
                      label = NULL,
                      group = 'default',
                      ...) {
  .paramFactory(
    name,
    label,
    'Date',
    group,
    tagFactory = function(...) {
      inputTag <- tag('input', c(list(type = 'date'), list(...)))
    },
    ...
  )
  
}

#'
#' Creates text input control
#'
#' @export
#'
textParam <- function(name,
                      label = NULL,
                      group = 'default',
                      ...) {
  .paramFactory(
    name,
    label,
    'character',
    group,
    tagFactory = function(...) {
      inputTag <- tag('input', c(list(type = 'text'), list(...)))
    },
    ...
  )
}

#'
#' Creates numeric slider input control
#'
#' @export
#'
numericSliderParam <-
  function(name,
           label = NULL,
           min = NA,
           max = NA,
           group = 'default',
           ...) {
    numericParam(name,
                 label,
                 min,
                 max,
                 group,
                 type = 'range',
                 class = 'form-control slider',
                 ...)
  }
#'
#' Creates numeric input control
#'
#' @export
#'
numericParam <-
  function(name,
           label = NULL,
           min = NA,
           max = NA,
           group = 'default',
           ...) {
    .paramFactory(
      name,
      label,
      'numeric',
      group,
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
#' @export
#'
selectParam <-
  function(name,
           label = NULL,
           choices = list(),
           group = 'default',
           ...) {
    .paramFactory(
      name,
      label,
      'character',
      group,
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
      rToTagValueMapper = .rToUIControlValueMapper('select', choices),
      ...
    )
  }

#'
#' Creates radio buttons group input control
#'
#' @export
#'
choiceParam <-
  function(name,
           label = NULL,
           choices = list(),
           group = 'default',
           ...) {
    .paramFactory(
      name,
      label,
      'character',
      group,
      tagFactory = function(...) {
        res <- tag('div', list(...))
        res$attribs[.rcloudParamsAttr('radio-group-name')] = name
        res
      },
      rToTagValueMapper = .rToUIControlValueMapper('radio', choices),
      ...
    )
  }

#'
#' Creates checkbox input control
#'
#' @export
#'
checkboxParam <-
  function(name,
           label = NULL,
           group = 'default',
           ...) {
    .paramFactory(
      name,
      label,
      'logical',
      group,
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
      rToTagValueMapper = .rToUIControlValueMapper('checkbox'),
      ...
    )
  }
